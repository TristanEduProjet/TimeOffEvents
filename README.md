This is a starter project to use in an F# coding assignment working with event-sourcing and modeling a workflow.

It relies on .NET core and references Expecto as a testing library/runner.

Just clone the repo or download it, and get started coding and testing in F#!

To get started, just dotnet run in the folder to run the Expecto tests.

- - - -

## For (re-)install projet after pull/clone/retreive
```bash
dotnet restore
```
## For install web dependencies
You need *yarn* installed.
```bash
yarn install
```

## For launch tests
```bash
cd src/test/Logic.Tests
dotnet run
```

## For launch HTTP serveur
```bash
cd src/app/Server
dotnet run
```
