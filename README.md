This package provides head normal form strict versions of some
standard Haskell concurrency abstractions (`MVars`, `Chans`),
which provide control over where evaluation takes place
not offered by the default lazy types.
This may be useful for deciding when and where evaluation occurs,
leading to improved time or space use, depending on the circumstances.
