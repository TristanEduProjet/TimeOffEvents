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
You need *yarn* and *npx* installed.
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

## For launch HTTP client
```bash
cd src/app/Client
npx webpack-dev-server
```

## For devs
you can also use `dotnet watch run` instead of `dotnet run` for automatically re-run when code is modified.


# Server API

| Users         |  
+---------------+  
| manager       |  
| employee[1;5] |  
+---------------+  
_The **login** and **password** are the username._

Routes :
* localhost:5000/api/
    * POST /users/login/  
        Params
        ```json
        {
            "UserName": "manager",
            "Password": "manager",
            "PasswordId": "c76fe0c3-9fdf-4ba6-adf6-72eeae00abd6"
        }
        ```
        Response
        ```json
        {
            "UserName":"manager",
            "User":"Manager",
            "Token":"ey[...].f-CN2[...].okq5UO7[...].3_g5r5[...]"
        }
        ```
    * /timeoff/  
        Headers
        ```
        authorization: Bearer <Token>
        ```
        * GET /user-balance/<_username_>
        * POST /request/
        * POST /validate-request/
