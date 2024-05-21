# par(mfrow=c(1,2))
psychictest = function(n){
  par(mfrow=c(1,2))
  right = 0
  wrong = 0
  x = c(0,1,-2,2,-1,0)
  y = c(4,0,3,3,0,4)
  J = 1
  while(J <= n){
    answer = readline("L or R? ")
    if(answer == "L"){
      coinflip = sample(c("L", "R"), 1)
      if(coinflip == "L"){
        right = right + 1
        plot(x, y, type="l", main=paste(right, "correct!"))
        plot(x, y, type="n")
        J = J + 1
      } else {
        wrong = wrong + 1
        plot(x, y, type="n", main=paste(wrong, "WRONG!"))
        plot(x, y, type="l")
        J = J + 1
      }
    } else if(answer == "R"){
      coinflip = sample(c("L", "R"), 1)
      if(coinflip == "L"){
        wrong = wrong + 1
        plot(x, y, type="l")
        plot(x, y, type="n", main=paste(wrong, "WRONG!"))
        J = J + 1
      } else {
        right = right + 1
        plot(x, y, type="n")
        plot(x, y, type="l", main=paste(right, "correct!"))
        J = J + 1
      }
    } else {print("You broke the game...")}
  }
  print(paste("Your score:", right, "out of", right + wrong))
}

multitest = function(n){
  par(mfrow=c(1,1))
  sides = sample(c("Left", "Right"), n, replace=TRUE)
  plot(c(-2,2), c(-n,-1), type="n", xaxt="n", yaxt="n",
    xlab="", ylab="", main=paste("Psychic test,",n,"trials"))
  for(J in 1:n){
    if(sides[J]=="Left"){text(-1,-J,paste(J,"Left"))
    } else {
    text(1,-J,paste(J,"Right"))}
  }
}
