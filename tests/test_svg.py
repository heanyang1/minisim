#!/usr/bin/env python3
"""Unit tests for wavedrom2svg.py (run with: python3 -m unittest discover -s tests)."""

import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

import wavedrom2svg as wd  # noqa: E402


class Expand(unittest.TestCase):
    def test_bit_levels(self):
        self.assertEqual(
            wd.expand({"wave": "01xz"}),
            [("0", None, True), ("1", None, True), ("x", None, True), ("z", None, True)],
        )

    def test_hl(self):
        self.assertEqual(
            wd.expand({"wave": "hl"}),
            [("1", None, True), ("0", None, True)],
        )

    def test_dot_repeats_previous(self):
        self.assertEqual(
            wd.expand({"wave": "0.1"}),
            [("0", None, True), ("0", None, False), ("1", None, True)],
        )

    def test_dot_at_start_is_x(self):
        self.assertEqual(wd.expand({"wave": ".0"})[0], ("x", None, False))

    def test_data_states_consume_data(self):
        self.assertEqual(
            wd.expand({"wave": "2..=", "data": ["aa", "bb"]}),
            [("d", "aa", True), ("d", "aa", False), ("d", "aa", False), ("d", "bb", True)],
        )

    def test_missing_data_entry(self):
        self.assertEqual(wd.expand({"wave": "2", "data": []}), [("d", "?", True)])

    def test_clock_chars(self):
        self.assertEqual(
            wd.expand({"wave": "p.P."}),
            [("p", None, True), ("p", None, False),
             ("P", None, True), ("P", None, False)],
        )

    def test_gap(self):
        self.assertEqual(wd.expand({"wave": "0|1"})[1], ("g", None, True))

    def test_unsupported_char(self):
        with self.assertRaises(ValueError):
            wd.expand({"wave": "q"})

    def test_digit_data_states(self):
        for ch in "3456789":
            self.assertEqual(wd.expand({"wave": ch, "data": ["v"]}), [("d", "v", True)])


class Render(unittest.TestCase):
    def render(self, js):
        return wd.render(js)

    def test_signal_name_and_ticks(self):
        svg = self.render({"head": {"tick": 0},
                           "signal": [{"name": "clk", "wave": "0101"}]})
        self.assertIn(">clk<", svg)
        self.assertIn(">1<", svg)
        self.assertIn(">4<", svg)

    def test_no_tick_ruler_without_head(self):
        svg = self.render({"signal": [{"name": "a", "wave": "0101"}]})
        self.assertNotIn(">2<", svg)

    def test_data_text_rendered(self):
        svg = self.render({"signal": [{"name": "bus", "wave": "2..", "data": ["3f"]}]})
        self.assertIn(">3f<", svg)

    def test_fresh_data_starts_new_box(self):
        svg = self.render({"signal": [{"name": "bus", "wave": "2.2", "data": ["1", "2"]}]})
        self.assertIn(">1<", svg)
        self.assertIn(">2<", svg)

    def test_x_box_drawn(self):
        svg = self.render({"signal": [{"name": "a", "wave": "x"}]})
        self.assertIn("#f8d7da", svg)  # x box fill

    def test_bit_wave_is_one_path(self):
        svg = self.render({"signal": [{"name": "a", "wave": "0101"}]})
        self.assertEqual(svg.count("<path"), 1)

    def test_transitions_make_one_contiguous_path(self):
        svg = self.render({"signal": [{"name": "a", "wave": "0011"}]})
        self.assertEqual(svg.count("<path"), 1)
        self.assertIn("L", svg)  # polyline, not a single horizontal line

    def test_x_run_breaks_path(self):
        svg = self.render({"signal": [{"name": "a", "wave": "01x10"}]})
        self.assertEqual(svg.count("<path"), 2)

    def test_clock_wave(self):
        svg = self.render({"signal": [{"name": "c", "wave": "p."}]})
        self.assertEqual(svg.count("<path"), 1)

    def test_head_and_foot_text(self):
        svg = self.render({"head": {"tick": 0, "text": "TITLE"},
                           "signal": [{"name": "a", "wave": "01"}],
                           "foot": {"text": "FOOT"}})
        self.assertIn(">TITLE<", svg)
        self.assertIn(">FOOT<", svg)

    def test_empty_diagram(self):
        svg = self.render({"signal": []})
        self.assertTrue(svg.startswith("<?xml"))

    def test_escaping(self):
        svg = self.render({"signal": [{"name": "a<b", "wave": "01"}]})
        self.assertIn("a&lt;b", svg)


if __name__ == "__main__":
    unittest.main()
