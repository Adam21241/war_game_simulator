# WAR GAME SIMULATOR (2 players only)
# Numeration: 2 - 10, 11 (J), 12 (Q), 13 (K), 14 (A)

# creation of a deck
deck <- c()
for (i in 1:52) {
  deck[i] <- (i + 7) %/% 4
}
print(deck)

# set seed (to get repetable results on other machine)
set.seed(91)

# shuffling of the deck
deck <- sample(deck)
print(deck)

# division of the deck between 2 players
player_1 <- deck[c(TRUE, FALSE)]
player_2 <- deck[c(FALSE, TRUE)]
print(player_1)
print(player_2)

# set max number of iterations, after which the winner is a player with bigger number of cards
max_i = 1000

# number of cards of player_1 in each iteration
num_of_cards_1 <- c() 

# start game 
i = 1

while (i <= max_i) {
  print("Iteration:")
  print(i)
  print("Cards of player_1:")
  print(player_1)
  print("Cards of player_2")
  print(player_2)
  print("Number of cards of player_1:")
  print(length(player_1))
  print("Number of cards of player_2:")
  print(length(player_2))
  cat("\n")
  
  if (length(player_1) == 0) {
    print(paste("The winner is player_2 after", i, "iterations."))
    break
  } else if (length(player_2) == 0) {
    print(paste("The winner is player_1 after", i, "iterations."))
    break
  }  
  
  if (player_1[1] > player_2[1]) {
    player_1[length(player_1) + 1] <- player_1[1]
    player_1 <- player_1[-1]
    player_1[length(player_1) + 1] <- player_2[1]
    player_2 <- player_2[-1]
  } else if (player_1[1] < player_2[1]) {
    player_2[length(player_2) + 1] <- player_2[1]
    player_2 <- player_2[-1]
    player_2[length(player_2) + 1] <- player_1[1]
    player_1 <- player_1[-1]
  } else {
    temp_stack <- c(player_1[1], player_2[1])
    player_1 <- player_1[-1]
    player_2 <- player_2[-1]
    while (TRUE) {
      
      if (length(player_1) < 2) {
        length(player_1) = 0
        break
      }
      else if (length(player_2) < 2) {
        length(player_2) = 0
        break
      }
      
      temp_stack <- append(temp_stack, player_1[1:2])
      temp_stack <- append(temp_stack, player_2[1:2])
      player_1 = player_1[-1 : -2]
      player_2 = player_2[-1 : -2]
      
      if (temp_stack[length(temp_stack) - 2] > temp_stack[length(temp_stack)]) {
        player_1 <- append(player_1, temp_stack)
        break
      }
      else if (temp_stack[length(temp_stack)] > temp_stack[length(temp_stack) - 2]) {
        player_2 <- append(player_2, temp_stack)
        break
      }
    }
  }
  num_of_cards_1[i] <- length(player_1)
  i = i + 1
}

if (i > max_i) {
  print(paste("No result after", max_i, "iterations."))
  if (length(player_1) > length(player_2)) {
    print(paste("The winner is player_1 having", num_of_cards_1[i - 1],  "cards."))
  } else if (length(player_2) > length(player_1)) {
    print(paste("The winner is player_2 having", (52 - num_of_cards_1[i - 1]),  "cards."))
  } else {
    print("There is a draw.")
  }
}

# plot (number of cards of player_1 in each iteration)
plot(num_of_cards_1, type = 'p', pch = 20, xlab = "Iteration", ylab = "Number of cards", main = "Number of cards of player_1 in each iteration", ylim = c(0,52), las = 1)
