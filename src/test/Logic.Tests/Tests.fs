module TimeOff.Tests

open Expecto
open System

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let AndDateIs (year: int, month: int, day: int) (events: RequestEvent list, user: User) = events, user, DateTime(year, month, day)
let When (command: Command) (events: RequestEvent list, user: User, today: DateTime) = events, user, today, command
let Then expected message (events: RequestEvent list, user: User, today: DateTime, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide today userRequestsState user command
    Expect.equal result expected message

open System
open TimeOff

[<Tests>]
let BoundaryTests = 
  testList "Boundary tests" [ //thank auto-compare !
    test "A Boundary equals itself" {
      let request:Boundary = { Date = DateTime(2018, 10, 1); HalfDay = AM }

      Expect.isTrue (request = request) "A boundary should equals with istself"
    }

    test "A Boundary equals identic instance" {
      let request1:Boundary = { Date = DateTime(2018, 10, 1); HalfDay = AM }
      let request2:Boundary = { Date = DateTime(2018, 10, 1); HalfDay = AM }

      Expect.isTrue (request1 = request2) "A boundary should equals another instance"
    }

    test "A Boundary less another" {
      let request1:Boundary = { Date = DateTime(2018, 10, 1); HalfDay = AM }
      let request2:Boundary = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      let request3:Boundary = { Date = DateTime(2018, 10, 2); HalfDay = AM }

      Expect.isTrue (request1 < request2) "A boundary should be less halfday"
      Expect.isTrue (request1 < request3) "A boundary should be less next halfday"
      Expect.isTrue (request2 < request3) "A boundary should be less next day"
    }

    test "A Boundary greater another" {
      let request1:Boundary = { Date = DateTime(2018, 10, 1); HalfDay = AM }
      let request2:Boundary = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      let request3:Boundary = { Date = DateTime(2018, 10, 2); HalfDay = AM }

      Expect.isTrue (request2 > request1) "A boundary should be greater halfday"
      Expect.isTrue (request3 > request1) "A boundary should be greater next day"
      Expect.isTrue (request3 > request2) "A boundary should be greater next halfday"
    }
  ]

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "A request overlaps with another" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 4); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 3); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request1 request2) "A request should overlap with bigger"
      Expect.isTrue (Logic.overlapsWith request2 request1) "A request should overlap with smaller"
    }

    test "A request overlaps partialy with another" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 3); HalfDay = AM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 4); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request1 request2) "A request should start after another"
      Expect.isTrue (Logic.overlapsWith request2 request1) "A request should end before another"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
      Expect.isFalse (Logic.overlapsWith request2 request1) "The requests don't overlap (inverse)" //double check
    }

    test "Requests on same day without overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = AM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = PM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
      Expect.isFalse (Logic.overlapsWith request2 request1) "The requests don't overlap (inverse)" //double check
    }

    test "Requests on same day without overlap (extend test)" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 2); HalfDay = AM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 2); HalfDay = PM }
        End = { Date = DateTime(2018, 10, 3); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }
  ]

[<Tests>]
let overlapSeqTests = 
  testList "Overlap seq tests" [
    test "Request overlaps when superpose _1" {
      let requests: TimeOffRequest seq = seq[
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
          End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2018, 10, 3); HalfDay = AM }
          End = { Date = DateTime(2018, 10, 4); HalfDay = PM }
        }
      ]
      let request: TimeOffRequest = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 4); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 5); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWithAnyRequest requests request) "A request should overlap"
    }

    test "Request overlaps when superpose _2" {
      let requests: TimeOffRequest seq = seq[
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
          End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2018, 10, 3); HalfDay = AM }
          End = { Date = DateTime(2018, 10, 4); HalfDay = PM }
        }
      ]
      let request: TimeOffRequest = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 9, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 5); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWithAnyRequest requests request) "A request should overlap"
    }
    
    test "Request overlaps when superpose _3" {
      let requests: TimeOffRequest seq = seq[
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
          End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2018, 10, 3); HalfDay = AM }
          End = { Date = DateTime(2018, 10, 4); HalfDay = PM }
        }
      ]
      let request: TimeOffRequest = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 9, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 1); HalfDay = AM }
      }

      Expect.isTrue (Logic.overlapsWithAnyRequest requests request) "A request should overlap with istself"
    }

    test "Request don't overlaps" {
      let requests: TimeOffRequest seq = seq[
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
          End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2018, 10, 3); HalfDay = AM }
          End = { Date = DateTime(2018, 10, 4); HalfDay = PM }
        }
      ]
      let request1: TimeOffRequest = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 5); HalfDay = AM }
        End = { Date = DateTime(2018, 10, 5); HalfDay = PM }
      }
      let request2: TimeOffRequest = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 9, 30); HalfDay = AM }
        End = { Date = DateTime(2018, 9, 30); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWithAnyRequest requests request1) "A request should not overlap"
      Expect.isFalse (Logic.overlapsWithAnyRequest requests request2) "A request should not overlap"
    }

    test "Request don't overlaps between" {
      let requests: TimeOffRequest seq = seq[
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2018, 10, 1); HalfDay = AM }
          End = { Date = DateTime(2018, 10, 2); HalfDay = AM }
        };
        {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2018, 10, 3); HalfDay = AM }
          End = { Date = DateTime(2018, 10, 4); HalfDay = PM }
        }
      ]
      let request: TimeOffRequest = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 10, 2); HalfDay = PM }
        End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWithAnyRequest requests request) "A request should not overlap"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> AndDateIs (2018, 12, 3)
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }
    test "A request in the past cannot be created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 11, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 11, 28); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> AndDateIs (2018, 12, 3)
      |> When (RequestTimeOff request)
      |> Then (Error "The request starts in the past") "The request should not have been created"
    }
    //TODO: validé reqête qui n'existe pas
    //TODO: validé requête en n'étant pas manager
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> AndDateIs (2018, 12, 3)
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }
  ]

[<Tests>]
let cancellationTests =
  testList "Cancellation tests" [
    test "A request is cancelled" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> AndDateIs (2018, 12, 3)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
    }
    test "A validated request is cancelled" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2018, 12, 28); HalfDay = AM }
        End = { Date = DateTime(2018, 12, 28); HalfDay = PM } }

      Given [ RequestCreated request; RequestValidated request ]
      |> ConnectedAs (Employee "jdoe")
      |> AndDateIs (2018, 12, 3)
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCancelled request]) "The request should have been cancelled"
    }
  ]
