#!/bin/bash

HOST="$1"

scp project.clj "$HOST:./dan-bot/project.clj"
scp src/dan_bot/core.clj "$HOST:./dan-bot/src/dan_bot/core.clj"
scp src/dan_bot/nlp.clj "$HOST:./dan-bot/src/dan_bot/nlp.clj"
scp config.edn "$HOST:./dan-bot/config.edn"
