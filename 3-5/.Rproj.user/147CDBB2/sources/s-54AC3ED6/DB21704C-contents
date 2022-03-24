test_vector <- c(1:20)

tong <- function(x){
  total = 0
  for(i in 1:length(x)){
    total = total + x[i]
  }
  return(total)
}
tong(test_vector)

trungbinh <- function(x){
  return (sum(x)/length(x))
}


trungbinh(test_vector)

#Phương sai

phuongsai <-function(x){
  total = 0
  binhphuong <- (x-trungbinh(x))^2
  for (i in 1:length(x)){
    total = total + binhphuong[i]
  }
  ketqua <- total/(length(x)-1)
  return(ketqua)
}
print(phuongsai(test_vector))

#Độ lệch chuẩn
lechchuan <- function(x){
  ketqua <- sqrt(phuongsai(x))
  return(ketqua)
}
paste0("Hàm lệch chuẩn: ",lechchuan(test_vector))
paste0("Hàm sd ",sd(test_vector))

# Max
benhat <- function(x){
  Min <- x[1]
  for(i in 1:length(x)){
    if(Min > x[i]){
      Min = x[i]
    }
  }
  return (Min)
}
benhat(test_vector)
#Tính hệ số tương quan



HeSoTuongQuan <- function(x,y){
  return((phuongsai(x)*phuongsai(y))/(lechchuan(x)*lechchuan(y)))
  
}
x1 <- c(1:5)
y1 <- c(6:10)
HeSoTuongQuan(x1, y1)
