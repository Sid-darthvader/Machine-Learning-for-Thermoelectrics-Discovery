actualy = [0.253177, 0.691056, 1.072250, 1.455980, 1.468220, 1.492000,2.324460, 2.613060, 2.642000, 2.644620, 2.757830, 2.954230,3.181580, 3.300190, 3.441490, 3.636130, 3.693970, 4.089450,4.118380, 4.236740, 4.257790, 4.270980, 4.559470, 4.752600,4.760890, 4.850830]
predictedy = [0.5004733, 0.6474590, 0.9278226, 1.2665538, 1.1146742, 1.3447473, 1.9771825, 3.2427430, 2.9462585, 2.6774757, 2.8064594, 2.5645151,3.3311577, 3.6540430, 3.5546067, 3.6982694, 4.4322615, 3.9495337,4.1286960, 4.2435560, 4.1747398, 4.2268448, 4.5925236, 4.5117617,4.2175183, 4.3869047]
n = length(actualy);
firstRMSEdatanumber = 10;
coverageRMSE = zeros( n-firstRMSEdatanumber+1, 2);
for i = firstRMSEdatanumber : n
    coverageRMSE( i-firstRMSEdatanumber+1, : ) = [ i/n sqrt( sum((actualy(1:i)-predictedy(1:i)).^2) / n ) ];
end
% calculation of p%-AUCR
p = 10/100;
indexofcoveragelowerthanp = find( coverageRMSE(:,1) <= p/100 );
pAUCR = (sum( coverageRMSE(indexofcoveragelowerthanp,2))-(coverageRMSE(1,2)+coverageRMSE(indexofcoveragelowerthanp(end),2))/2 )*1/n;