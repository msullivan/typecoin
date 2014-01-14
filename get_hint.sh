#!/bin/sh

TXID=$(echo $1 | cut -d: -f1)
OUT_IDX=$(echo $1 | cut -d: -f2)

LINE=$(curl -s http://blockexplorer.com/tx/$TXID | grep "name=\"o$OUT_IDX\"")

# XXX: do a check for it *not* being redeemed, probs
SPENDING_TX=$(echo $LINE | cut -d'/' -f3 | cut -d'#' -f1)

LINE=$(curl -s http://blockexplorer.com/tx/$SPENDING_TX | grep "Appeared in")
BLOCK=$(echo $LINE | cut -d" " -f5 | cut -d'<' -f1)

echo $BLOCK $SPENDING_TX
