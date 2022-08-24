# coinmarket-dashboard

Haskell CLI crypto coins market prices dashboard. 

**Note: this only inital beta(subject to change)**
## Main Features 

Live market data refreshed every `60` seconds from CoinMarketCap.

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

### Run Unit Tests

```bash
cabal test
```

## Contributors 

Yeramin (@ysfdev), Kenley (@registerzero), Andres (@OneHoax)
