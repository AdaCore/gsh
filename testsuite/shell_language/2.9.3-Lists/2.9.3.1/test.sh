#!/bin/sh

false && echo foo || echo bar
true || echo foo && echo bar
