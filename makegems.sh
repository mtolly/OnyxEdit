#!/bin/bash
haml data/gems.haml data/gems.svg
inkscape data/gems.svg --export-png=data/gems.png