degMinSec2decimal = function (deg, min, sec) {
  degrees = as.numeric(deg)
  minutes = as.numeric(min)
  seconds = as.numeric(sec)

  if(degrees < 0){
    return(degrees - minutes/60 - seconds/3600)
  } else {
    return(degrees + minutes/60 + seconds/3600)
  }
}

degMin2decimal = function (deg, min, min_thousandths = NA) {
  degrees = as.numeric(deg)
  minutes = as.numeric(min)

  if(!is.na(min_thousandths)){
    thousandths = as.numeric(min_thousandths)

    minutes = minutes + thousandths/1000
  }

  if(degrees < 0){
    return(degrees - minutes/60)
  } else {
    return(degrees + minutes/60)
  }
}
