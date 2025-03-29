% Steven Maere, 2025
%
% function calculating piecewise line y and integer ploidy boundaries bnd1,2,3 in the range x for parameters prm
% prm(1) = 2C average cross-sectional area, prm(2) = ploidy scaling factor, 
% prm(3) = real-valued 2C-4C boundary, prm(4) = real-valued 4C-8C boundary, prm(5) = real-valued 8C-16C boundary. 

function [y,bnd1,bnd2,bnd3] = piecewiseLine(x,prm)

y = zeros(size(x));
bnd1 = 1;
bnd2 = 1;
bnd3 = 1;
for i = 1:length(x)
    if x(i) < prm(3)
        y(i) = prm(1) ;
        bnd1 = bnd1+1;
        bnd2 = bnd2+1;
        bnd3 = bnd3+1;
    elseif x(i) < prm(4)
        y(i) = prm(2)*prm(1) ;
        bnd2 = bnd2+1;
        bnd3 = bnd3+1;
    elseif x(i) < prm(5)
        y(i) = (prm(2)^2)*prm(1) ;
        bnd3 = bnd3+1;
    else
        y(i) = (prm(2)^3)*prm(1) ;
    end
end
end
    