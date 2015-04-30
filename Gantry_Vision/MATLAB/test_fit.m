
!clc
!clear

data = dlmread('./1.txt','\t');
f = log(data(:,1)); #focus
h = data(:,2);      #height
N = size(f)(1);
#G = H + A*np.exp(-((x-mu)/sigma)**2)
#H: c(1)
#A: c(2)
#mu: c(3)
#sigma: c(4)



f_max = -10;
h_max = 0;
for i = 1:N
  if f(i) > f_max
    f_max = f(i);
    h_max = h(i);
  end
end


c_guess = [min(f); max(f)-min(f); h_max; .05];

[c,fval,info,output]=fsolve(@(c)( c(1) + c(2).*exp(-((h-c(3))./c(4)).^2) -f),c_guess);


f_calc = c(1) + c(2).*exp(-((h-c(3))./c(4)).^2);
error = sum((f-f_calc).^2)/N;
plot(h,f,'r.');
hold on;
plot(h,f_calc,'b.');

error
c(3)