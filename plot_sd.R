library("sp")
dat<-data.frame(t=seq(0,2*pi,by=0.1))
xcor<-function(t) 16*sin(t)^3
ycor<-function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
dat$y=ycor(dat$t)
dat$x=xcor(dat$t)
te<-"01010100010001010010000001000101010110000101010001010010010000011101000101001111"
xpoi_inf<--17
xpoi_sup<-17
xpoi<-seq(xpoi_inf,xpoi_sup,by = 1)
se<-paste(toupper(substr(month.name[9],4,4)),LETTERS[3^2-2^2],
intToUtf8(atan(1/sqrt(3))*180/pi+2),intToUtf8(acos(log(1))*180/pi-9),
intToUtf8(acos(log(1))*180/pi-5),intToUtf8(acos(log(1))*180/pi-17),
LETTERS[3^2-2^2],toupper(substr(month.name[2],4,4)),
intToUtf8(acos(exp(0)/2)*180/pi+2^4+3),sep=intToUtf8(0))
ypoi_inf<--17
ypoi_sup<-12
ypoi<-seq(ypoi_inf,ypoi_sup,by = 1)
ca<-paste(toupper(substr(month.name[12],3,3)),LETTERS[3^2-2^3],
substr(month.name[3],1,1),intToUtf8(acos(log(1))*180/pi-17),
toupper(substr(month.name[7],3,3)),LETTERS[3^2-2^3],sep=intToUtf8(0))
lim_se<-round(nchar(se)/2)
lim_ca<-round(nchar(ca)/2)
mod_se<-nchar(se)%%2-1
mod_ca<-nchar(ca)%%2-1
with(dat, plot(x,y,type="l",col="white"))
for (row in ypoi) {
	res<-point.in.polygon(xpoi, rep(row, xpoi_sup-xpoi_inf), dat$x, dat$y)
	i<-xpoi_inf
	j<-1
	k<-1
	l<-1
	for (col in res) {
		if (col) {
			if (row == 0 && i >= -lim_se && i <= lim_se + mod_se) {
				pr<-substring(se,j,j)
				if (pr==intToUtf8(atan(1/sqrt(3))*180/pi+2) || pr=="") {
					text(i,row,substring(te,l,l),col='red',cex=1)
					l<-l+1
				}
				else {
					text(i,row,pr,col='blue',cex=1)
				}
				j<-j+1
			}
			else if (row == -1 && i >= -lim_ca && i <= lim_ca + mod_ca) {
				pr<-substring(ca,k,k)
				if (pr==intToUtf8(atan(1/sqrt(3))*180/pi+2) || pr =="") {
					text(i,row,substring(te,l,l),col='red',cex=1)
					l<-l+1
				}
				else {
					text(i,row,pr,col='blue',cex=1)
				}
				k<-k+1
			}
			else {
				text(i,row,substring(te,l,l),col='red',cex=1)
				l<-l+1
			}
			if (substring(te,l,l)== "") {
				l<-1
			}
		}
		i<-i+1
	}
}
