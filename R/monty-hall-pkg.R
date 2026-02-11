#' @title
#'   Create a new \emph{Monty Hall} game.
#'
#' @description
#'   `Create_Monty_Hall_Game()` creates a new game consisting of
#'    a specified number of doors with cars and goats randomly
#'    placed behind them.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are doors for a contestant
#'   to choose from, some of which have cars behind them and the rest
#'   have goats. The contestant selects a door, then Monty (Monty Hall)
#'   opens door(s) to reveal goat(s) and/or car(s), and then the contestant
#'   is given an opportunity to stay with their original selection
#'   or switch to another unopened door. There was a famous
#'   debate - largely started by Marilyn vos Savant's response in
#'   a 1990 Parade column - about whether it was optimal to stay or switch
#'   when given the option to switch, so this simulation was created
#'   to test both strategies. This generalized version allows for
#'   variable numbers of doors and cars to explore extensions of
#'   the classic problem.
#'
#' @param Num_Doors This is an integer value. The number of doors in the game.
#'  The default is 3, and it \bold{must} be at least 3 to ensure the game
#'  can be played.
#' @param Num_Cars This is also an integer value. The number of cars
#'  in the game. (I hope they're nice.) The default is 1. It \bold{must} be
#'  at least 1 and at most (Num_Doors - 2) to ensure a
#'   sufficient number of goats for Monty's reveal.
#'
#' @return The function returns a character vector of length Num_Doors
#'   containing "car" and "goat" values indicating the positions of
#'   goats \emph{and} cars behind each door.
#'
#' @examples
#'   # Replicates the classic 3-door game with 1 car
#'   Create_Monty_Hall_Game()
#'
#'   # Create a 5-door game with 2 cars
#'   Create_Monty_Hall_Game(Num_Doors = 5, Num_Cars = 2)
#'
#'   # Create a 10-door game with 3 cars
#'   Create_Monty_Hall_Game(Num_Doors = 10, Num_Cars = 3)
#'
#' @export
Create_Monty_Hall_Game <- function(Num_Doors = 3, Num_Cars = 1)
{
  stopifnot(Num_Doors >= 3)
  stopifnot(Num_Cars >= 1)
  stopifnot(Num_Cars <= Num_Doors - 2)

  Monty_Hall_Game <- sample(
    x = c(rep("car", Num_Cars), rep("goat", Num_Doors - Num_Cars)),
    size = Num_Doors,
    replace = FALSE
  )
  return(Monty_Hall_Game)
}



#' @title
#'   Select a door.
#'
#' @description
#'   `Door_Select()` randomly picks one of the available doors
#'   for the contestant's initial choice.
#'
#' @details
#'   The contestant makes their initial selection by randomly
#'   choosing one of the available doors (numbered 1 through Num_Doors),
#'    which is made \emph{before} any doors are opened by Monty.
#'   The selection is \emph{always} random across all available doors.
#'
#' @param Num_Doors This is an Integer value. The number of doors in
#'   the game, where  the default is 3, and it \bold{must} be at least 3.
#'
#' @return The function returns an integer between 1 and Num_Doors
#'   representing the door number selected by the contestant.
#'
#' @examples
#'   # Select a door in a standard 3-door game
#'   Door_Select()
#'
#'   # Select a door in a 10-door game
#'   Door_Select(Num_Doors = 10)
#'
#' @export
Door_Select <- function(Num_Doors = 3)
{
  stopifnot(Num_Doors >= 3)
  Doors <- 1:Num_Doors
  a.pick <- sample(Doors, size = 1)
  return(a.pick)
}



#' @title
#'   Monty opens door(s) to reveal goats and/or cars.
#'
#' @description
#'   `Open_The_Floodgates()` determines which door(s) Monty will open
#'   after the contestant makes their initial selection. Monty can
#'   reveal goats and/or cars based on the game configuration, generalizing
#'   the classic "Release_The_Goats" function from Lab 1.
#'
#' @details
#'   After the contestant selects a door, Monty (who knows what's
#'   behind each door) opens one or more of the remaining doors.
#'   Monty will \bold{never} open the door the contestant selected.
#'   In the classic game, Monty opens one goat door. This generalized
#'   version allows Monty to open multiple goat doors and/or car doors,
#'   enabling exploration of game variants such as where Monty
#'   reveals both a goat and a car. The function handles the edge case
#'   where sample() behaves unexpectedly with length-1 vectors
#'   by explicitly checking candidate vector lengths before sampling.
#'
#' @param game A character vector containing "car" and "goat" values
#'   representing the current game setup
#'   (created by `Create_Monty_Hall_Game()`).
#' @param a.pick An integer representing the door number that the
#'   contestant initially selected. Must be between 1 and Num_Doors.
#' @param Reveal_Goats This is an Integer value.
#'   The number of goat doors Monty will open, the default is 1.
#'   It must not exceed the number of available goat doors
#'   (excluding the contestant's pick).
#' @param Reveal_Cars This is also an Integer value. The number of car doors
#'   Monty will open, where the default is 0. It must not exceed the
#'   number of available car doors (excluding the contestant's pick).
#'
#' @return The function returns an integer vector containing the door
#'   number(s) that Monty opened.
#'   The length equals Reveal_Goats + Reveal_Cars.
#'
#' @examples
#'   # Standard game - host opens 1 goat door
#'   game <- Create_Monty_Hall_Game()
#'   pick <- Door_Select()
#'   Open_The_Floodgates(game, pick)
#'
#'   # Generalized game - host opens 1 goat and 1 car
#'   game <- Create_Monty_Hall_Game(Num_Doors = 5, Num_Cars = 2)
#'   pick <- Door_Select(Num_Doors = 5)
#'   Open_The_Floodgates(game, pick, Reveal_Goats = 1, Reveal_Cars = 1)
#'
#'   # Open 2 goat doors in a 10-door game
#'   game <- Create_Monty_Hall_Game(Num_Doors = 10, Num_Cars = 1)
#'   pick <- Door_Select(Num_Doors = 10)
#'   Open_The_Floodgates(game, pick, Reveal_Goats = 2, Reveal_Cars = 0)
#'
#' @export
Open_The_Floodgates <- function(game, a.pick, Reveal_Goats = 1,
                                Reveal_Cars = 0)
{
  Num_Doors <- length(game)
  stopifnot(a.pick %in% 1:Num_Doors)

  # Where the goats and cars at?
  Goat_Candidates <- setdiff(which(game == "goat"), a.pick)
  Car_Candidates <- setdiff(which(game == "car"), a.pick)

  # Ensure host can open the required number of doors
  stopifnot(length(Goat_Candidates) >= Reveal_Goats)
  stopifnot(length(Car_Candidates) >= Reveal_Cars)

  # sample() compliance enforcement for goat selection
  Opened_Goats <- if (Reveal_Goats == 0) {
    integer(0)
  } else if (length(Goat_Candidates) == 1) {
    Goat_Candidates
  } else {
    sample(Goat_Candidates, Reveal_Goats)
  }

  # Car selection with sample() handling
  Opened_Cars <- if (Reveal_Cars == 0) {
    integer(0)
  } else if (length(Car_Candidates) == 1) {
    Car_Candidates
  } else {
    sample(Car_Candidates, Reveal_Cars)
  }

  Opened.Doors <- c(Opened_Goats, Opened_Cars)

  stopifnot(!(a.pick %in% Opened.Doors))

  return(Opened.Doors)
}



#' @title
#'   Change door selection or stay.
#'
#' @description
#'   `Door_Change()` implements the contestant's final decision to
#'   either stay with their original door or switch to one of the
#'   remaining unopened doors.
#'
#' @details
#'   After Monty opens door(s), the contestant must decide
#'   whether to stay with their original selection or switch to
#'   one of the other unopened doors. If the contestant chooses to stay,
#'   the function returns their original pick. If the contestant chooses
#'   to switch, the function randomly selects from the remaining doors
#'   that are neither the original pick nor any of the opened doors.
#'   The function handles the edge case where sample() behaves
#'   unexpectedly with length-1 vectors.
#'
#' @param stay This is a logical value. If TRUE, the contestant stays with
#'   their original selection. If FALSE, the contestant switches to
#'   one of the remaining unopened doors. Default is TRUE.
#' @param Opened.Door This is an integer vector containing the door number(s)
#'   that Monty opened.
#' @param a.pick This is an integer representing the door number that the
#'   contestant originally selected.
#' @param Num_Doors This is an Integer. The total number of doors in the game.
#'   Default is 3, and it \bold{must} be at least 3.
#'
#' @return The function returns an integer between 1 and Num_Doors
#'   representing the contestant's final door selection.
#'
#' @examples
#'   # Contestant stays with original pick
#'   Door_Change(stay = TRUE, Opened.Door = 2, a.pick = 1)
#'
#'   # Contestant switches to remaining door
#'   Door_Change(stay = FALSE, Opened.Door = 2, a.pick = 1)
#'
#'   # Multiple doors opened, contestant switches
#'   Door_Change(stay = FALSE, Opened.Door = c(2, 4), a.pick = 1,
#'   Num_Doors = 5)
#'
#' @export
Door_Change <- function(stay = TRUE, Opened.Door, a.pick, Num_Doors = 3)
{
  stopifnot(Num_Doors >= 3)
  stopifnot(a.pick %in% 1:Num_Doors)
  stopifnot(all(Opened.Door %in% 1:Num_Doors))
  stopifnot(!(a.pick %in% Opened.Door))

  if (stay) {
    Last.Pick <- a.pick
  } else {
    Remaining <- setdiff(1:Num_Doors, c(a.pick, Opened.Door))
    stopifnot(length(Remaining) >= 1)
    # Handle sample() edge case with length-1 vectors
    Last.Pick <- if (length(Remaining) == 1) {
      Remaining
    } else {
      sample(Remaining, size = 1)
    }
  }
  return(Last.Pick)
}



#' @title
#'   Determine if the contestant won or lost.
#'
#' @description
#'   `Winner_Winner_Chicken_Dinner()` evaluates the contestant's final door
#'   selection and determines whether they won the car or got a goat.
#'
#' @details
#'   This function checks what prize is behind the contestant's
#'   final door selection. If the door contains a car, the contestant
#'   wins ("WINNER").
#'   If the door contains a goat, the contestant's a \bold{"LOSER"}.
#'
#' @param Last.Pick This is an integer representing the contestant's final
#'   door selection after deciding to stay or switch. Must be between
#'   1 and the number of doors.
#' @param game This is a character vector containing "car" and "goat" values
#'   representing the current game setup.
#'
#' @return The function returns a character string: "WINNER" if the
#'   contestant's final pick was a car, or "LOSER" if the contestant's
#'   final pick was a goat.
#'
#' @examples
#'   game <- c("goat", "car", "goat")
#'   Winner_Winner_Chicken_Dinner(Last.Pick = 2, game)
#'   Winner_Winner_Chicken_Dinner(Last.Pick = 1, game)
#'
#' @export
Winner_Winner_Chicken_Dinner <- function(Last.Pick, game)
{
  stopifnot(Last.Pick %in% 1:length(game))
  if (game[Last.Pick] == "car") {
    return("WINNER")
  } else {
    return("LOSER")
  }
}



#' @title
#'   Play a complete Monty Hall game.
#'
#' @description
#'   `Play_Game()` simulates one complete round of the Monty Hall
#'   problem, returning the outcomes for both stay and switch strategies.
#'
#' @details
#'   This function runs a complete simulation of the Monty Hall game
#'   by executing all steps in sequence: creating a new game with the
#'   specified number of doors and cars, having the contestant select
#'   a door, having Monty open the specified number of doors
#'   (goats or cars), and then determining the outcome for both possible
#'   strategies (staying with the original pick vs. switching to another
#'   door). The function returns a data frame, which shows  the outcome for
#'   each strategy, facilitating comparisons of the two approaches.
#'
#' @param Num_Doors This is an Integer value. The number of doors in the game.
#'  The default is 3.
#' @param Num_Cars This is an Integer value. The number of cars in the game.
#'  The default is 1.
#' @param Reveal_Goats This is an Integer value. The number of goat doors
#' Monty opens. The default is 1.
#' @param Reveal_Cars This is an Integer value. The number of car doors
#'  Monty opens. The default is 0.
#'
#' @return The function returns a data frame with two rows and two columns:
#'   \itemize{
#'     \item Strategy: A character column with values "stay" or "switch"
#'     \item Outcome: A character column with values "WINNER" or "LOSER"
#'   }
#'
#' @examples
#'   # Play a standard 3-door game
#'   Play_Game()
#'
#'   # Play a 5-door game with 2 cars
#'   Play_Game(Num_Doors = 5, Num_Cars = 2)
#'
#'   # Play a game where host reveals both a goat and a car (Part I)
#'   Play_Game(Num_Doors = 5, Num_Cars = 2, Reveal_Goats = 1, Reveal_Cars = 1)
#'
#' @export
Play_Game <- function(Num_Doors = 3, Num_Cars = 1, Reveal_Goats = 1,
                      Reveal_Cars = 0)
{
  New.Game <- Create_Monty_Hall_Game(Num_Doors, Num_Cars)
  First.Pick <- Door_Select(Num_Doors)

  Opened.Doors <- Open_The_Floodgates(New.Game, First.Pick, Reveal_Goats,
                                      Reveal_Cars)

  Final.Pick.Stay <- Door_Change(stay = TRUE, Opened.Doors, First.Pick,
                                 Num_Doors)
  Final.Pick.Switch <- Door_Change(stay = FALSE, Opened.Doors, First.Pick,
                                   Num_Doors)

  Outcome.Stay <- Winner_Winner_Chicken_Dinner(Final.Pick.Stay, New.Game)
  Outcome.Switch <- Winner_Winner_Chicken_Dinner(Final.Pick.Switch, New.Game)

  Game.Results <- data.frame(
    Strategy = c("stay", "switch"),
    Outcome = c(Outcome.Stay, Outcome.Switch),
    stringsAsFactors = FALSE
  )
  return(Game.Results)
}



#' @title
#'   Run a simulation of multiple Monty Hall games.
#'
#' @description
#'   `Run_Simulation()` runs multiple simulations of the Monty Hall
#'   problem and calculates the win proportions for each strategy.
#'
#' @details
#'   This function runs the specified number of Monty Hall game
#'   simulations and aggregates the results. It uses replicate() to
#'   efficiently play N games, collects all results into a data frame,
#'   and then calculates the proportion of wins for each strategy
#'   (stay vs. switch). For the classic 3-door, 1-car game, the results
#'   demonstrate that switching wins approximately 2/3 of the time while
#'   staying wins approximately 1/3 of the time. As the number of doors, cars,
#'   and goats is variable, statistical outcomes for a variety of
#'   configurations can be explored.
#'
#' @param Num_Doors This is an Integer value. The number of doors in each game.
#'  The default is 3.
#' @param Num_Cars This is an Integer value. The number of cars in each game.
#'  The default is 1.
#' @param Reveal_Goats This is an Integer value. The number of goat doors
#'   Monty opens in each game. Its default is 1.
#' @param Reveal_Cars This is an Integer value. The number of car doors
#'   Monty opens in each game. The default is 0.
#' @param N The most powerful letter in statistics. This is an integer
#' specifying the number of games to simulate. The default is 100.
#' Larger values converge \emph{almost surely} to the true win probabilities.
#'
#' @return The function returns a named numeric vector with two elements:
#'   \itemize{
#'     \item Stay: The proportion of wins when using the stay strategy
#'     \item Switch: The proportion of wins when using the switch strategy
#'   }
#'
#' @examples
#'   # Run 100 standard games (default)
#'   Run_Simulation()
#'
#'   # Run 10000 games for more accurate results
#'   Run_Simulation(N = 10000)
#'
#'   # Simulate Part II from lab 2: 10-door, 2-car game
#'   Run_Simulation(Num_Doors = 10, Num_Cars = 2, N = 1000)
#'
#'   # Simulate Part I from lab 1: host reveals goat and car
#'   Run_Simulation(Num_Doors = 5, Num_Cars = 2, Reveal_Goats = 1,
#'   Reveal_Cars = 1, N = 1000)
#'
#' @export
Run_Simulation <- function(Num_Doors = 3, Num_Cars = 1, Reveal_Goats = 1,
                           Reveal_Cars = 0, N = 100)
{


  Sims <- replicate(N, Play_Game(Num_Doors, Num_Cars, Reveal_Goats,
                                 Reveal_Cars),
                    simplify = FALSE)

  Results <- dplyr::bind_rows(Sims)

  Props <- prop.table(table(Results$Strategy, Results$Outcome), margin = 1)

  return(c(Stay = Props["stay", "WINNER"],
           Switch = Props["switch", "WINNER"]))
}
