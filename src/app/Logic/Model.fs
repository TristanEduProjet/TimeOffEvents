namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | CancelRequest of UserId * Guid
    with
    member this.UserId : UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _) -> userId
        | CancelRequest (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestCancelled of TimeOffRequest
    with
    member this.Request : TimeOffRequest =
        match this with
        | RequestCreated request -> request
        | RequestValidated request -> request
        | RequestCancelled request -> request

// We then define the state of the system, and our 2 main functions `decide` and `evolve`
module Logic =

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest
        | UnknownState of TimeOffRequest //default, TODO: à supprimer une fois tout implementer
        with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | UnknownState request
            | PendingValidation request
            | Validated request -> request
        member this.IsActive =
            match this with
            | NotCreated -> false
            | UnknownState _
            | PendingValidation _
            | Validated _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = match event with
                              | RequestCreated request -> PendingValidation request
                              | RequestValidated request -> Validated request
                              | _ -> UnknownState event.Request
                              //TODO: requestCancelled ...
        userRequests.Add (event.Request.RequestId, newRequestState)

    let rec overlapsWith (request1:TimeOffRequest) (request2:TimeOffRequest) = //requests mustbe valide (.start <= .end)
        //if request1 > request2 then overlapsWith request2 request1
        if (request1.End <= request2.Start && request1.End <= request2.End && request1.End.HalfDay <> request2.Start.HalfDay) then false
        elif (request2.End <= request1.Start && request2.End <= request1.End && request1.End.HalfDay <> request2.Start.HalfDay) then false
        else true

    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) request =
        //Seq.sortBy (req -> req.Start) otherRequests |> Seq.pairwise |>
        Seq.exists (overlapsWith request) otherRequests

    let createRequest today activeUserRequests request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request -> Ok [RequestValidated request]
        | _ -> Error "Request cannot be validated"

    let cancelRequest requestState =
        match requestState with
        | PendingValidation request
        | Validated request ->  Ok [RequestCancelled request]
        | _ -> Error "Request cannot be cancelled"

    let decide (today: DateTime) (userRequests: UserRequestsState) (user: User) (command: Command) =
        match user with
        | Employee userId when userId <> command.UserId -> Error "Unauthorized" //check employee act on his own datas
        | _ -> match command with
                | RequestTimeOff request ->
                    let activeUserRequests = userRequests |> Map.toSeq
                                                          |> Seq.map (fun (_, state) -> state)
                                                          |> Seq.where (fun state -> state.IsActive)
                                                          |> Seq.map (fun state -> state.Request)
                    createRequest today activeUserRequests request
    
                | ValidateRequest (_, requestId) ->
                    if user <> Manager then
                        Error "Unauthorized"
                    else
                        validateRequest (defaultArg (userRequests.TryFind requestId) NotCreated)
    
                | CancelRequest (_, requestId) ->
                    if user = Manager then
                        Error "Unauthorized"
                    else
                        cancelRequest (defaultArg (userRequests.TryFind requestId) NotCreated)
