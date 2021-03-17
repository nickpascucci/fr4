#! /bin/bash

function cleanup() {
    rm *.profraw *.profdata
}

trap cleanup EXIT
trap cleanup SIGINT

LLVM_PROFILE_FILE=".fr4-%m.profraw" \
                 RUSTFLAGS="-Zinstrument-coverage" cargo +nightly test

OBJECTS=$( \
           for file in \
               $( \
                  RUSTFLAGS="-Zinstrument-coverage" \
                           cargo +nightly test --tests --no-run --message-format=json \
                      | jq -r "select(.profile.test == true) | .filenames[]" \
                      | grep -v dSYM - \
               ); \
           do \
               printf "%s %s " -object $file; \
           done \
       )

echo "OBJECTS=$OBJECTS"

cargo profdata -- merge \
      -sparse .fr4-*.profraw -o .fr4.profdata

cargo cov -- show \
      $OBJECTS \
      --instr-profile=.fr4.profdata \
      --use-color \
      --ignore-filename-regex='/.cargo/registry' \
      --ignore-filename-regex='/target' \
      --ignore-filename-regex='/rustc' \
      --Xdemangler=rustfilt \
      --format=html > /tmp/coverage.html \
      && open /tmp/coverage.html
