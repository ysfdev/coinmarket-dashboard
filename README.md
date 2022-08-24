# coinmarket-dashboard

CLI-based crypto coins market prices dashboard

## Main Features 

Live market data refreshed every `60` seconds

- Query info about some coin (current price, day high/low open/close, etc)
- Dashboard displaying latest top 15 coins

## Run App

```bash
cabal run coinmarket-dashboard
```

## Testing

### Install and configure unit test

```bash
cabal install && cabal configure --enable-tests
```

### Run Uni Tests

```bash
cabal test
```