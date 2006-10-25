#!/bin/sh

TEXINPUTS=".;../texsty//;"
export TEXINPUTS
platex $@
