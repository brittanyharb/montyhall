#' @title
#' 	Create a New Game.
#'
#' @description
#' 	`create_game()` randomly assigns a specified number of both goats and cars to the
#'	number
#'
#' @details
#' 	This funtion creates a new game for the monty hall problem. There are three doors: 
#' 	two goats and one car. this simulation gives you the opportunity to test the 
#' 	probabilities!
#' 
#'
#' @param x Numeric vector
#' 
#' @param values must be whole integers greater than zero
#'
#' @return 
#' 	Returns a length 3 charater vector showing the positions of the goats and the car 
#'
#' @examples
#' 	create_game()
#'
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}


#' @title
#' 	Select Door.
#'
#' @description
#' 	`select_door()` A door is randomly selected as the contestant choice. 
#'
#' @details
#'	This function assignes a random door as the contestant's first choice 
#' 	of door before anything is revealed. The contestant can only choose between door 1, 2, or 3. 
#'
#' @param x Numeric vector
#'
#' @return 
#' 	an integer that represents the number of the door that the participant selects.
#'
#' @examples
#' 	select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}


#' @title
#' 	Open the Goat Door.
#'
#' @description
#' 	`open_goat_door()` opens a door with a goat behind
#' 	it and that is also not the same door the contestant 
#' 	chose.
#' 
#' @details 
#' 	The opened door must have a goat behind it, 
#' 	meaning that the door must not have a car behind it.
#' 
#' @param ... the arguments are `game` and `a.pick`
#'
#' @return
#' 	This returns a number representing the number of 
#' 	the goat door that the host opened.
#' 
#' @example
#' 	open_goat_door(game = newGame, a.pick = firstDoor
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Changing Door Function.
#'
#' @description
#' 	`change_door()` the contestant is asked if they would like to change their 
#' 	selection to a different door or if they would like to keep their first choice.
#'
#' @details
#' 	a door is generated that represents the door that the 
#' 	contestant chose, whether they switched or stayed.
#' 
#' @param ... If the argument is `TRUE` it means that the contestant stayed
#' 	and `FALSE` means that the contestant switched doors.
#' 
#' @return
#' 	This function returns a number representing the number of the final door choice for the contestant.
#' 
#' @examples
#' 	change_door(stay = F, opened.door = 2, a.pick = 1)
#' 
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}


#' @title
#' 	Determine Winner.
#'
#' @description
#' 	`determine_winner()` returns the item behind the final door chosen, and assigns 
#' 	choosing the car as winning, and choosing the goat as losing.
#' 
#' @details
#' 	Provides final door and determines whether they won or lost.
#'
#' @param ... `final.pick` and `game` are the arguments
#'
#' @return
#' 	the function returns a "WIN" or "LOSE" based on whether or not the game was won.
#' 
#' @examples
#' 	determine_winner(final.pick = finalDoor, game = newGame)
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}

#' @title
#' 	Play the Game
#'
#' @description
#' 	`play_game()` combines the five functions above into this one function.
#'
#' @details
#' 	This function lists the corresponding outcomes for each strategy, which will vary each time.
#' 
#' @param ... No parameters
#'
#' @return
#' 	This returns a dataframe with columns `strategy` and `outcome`
#' 	strategy can be either stay or switch
#' 	outcome can be either win or lose
#'
#' @examples
#' 	play_game()
#' 
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )}



#' @title
#' 	Play the Game n times (by default, n=1000).
#'
#' @description
#' 	creates the game structure on a loop to examine the outcomes 
#' 	of running the game a multitude of times. Gives probability by 
#' 	playing the game as many times as needed using `play_n_games()`.
#'
#' @details
#' 	The default for the game is 1000 times, but it can be played n time
#' 	Corresponding outcomes are listed for each game.
#'
#' @param ... "n", or the number of times to run the loop
#'
#' @return
#' 	This function creates a dataframe that combines the outcomes of total games and lists then out of 1
#'
#' @examples
#' 	play_n_games(n=1000)
#' 
#' @export
play_n_games <- function( n=1000 )
{
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }
  results.df <- dplyr::bind_rows( results.list )
  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()
  return( results.df )
}