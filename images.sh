#!/bin/bash
runhaskell -Wall data/gems.hs > data/gems.svg
inkscape data/gems.svg --export-png=data/gems.png
inkscape data/staff.svg --export-png=data/staff.png
inkscape data/now.svg --export-png=data/now.png
