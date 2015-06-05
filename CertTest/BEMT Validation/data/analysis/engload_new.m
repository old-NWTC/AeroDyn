%engload_new.m

%This program reads the UAE *.eng files directly.

function [data] = engload_new(filnam, nchan, nsamp)

%================================================================

% %clear everything before starting
%  clear all;
%  
% %enter the file names from the keyboard
%  filnam = input('Enter file name (*.eng):  ','s');
%  
% %enter the number of channels (w/out error bit) from keyboard
%  nchan = input('Enter number of channels (see *.hd1 file):  ');
%  
% %enter number of samples to read from the keyboard
%  nsamp = input('Enter number of samples (see *.hd1 file):  ');
 
%================================================================

%open the file in read only mode, using little endian format
 fid = fopen(filnam,'r','ieee-le');

%proceed only if file was opened and ID returned
 if (fid ~= -1)

%    create data array and fill it with zeroes; this is faster
%    than creating the array one line at a time as data are read in
     data = zeros(nsamp, nchan);

%    create error bit array and fill it with zeroes; same reason
%    as above for data array
     errbit = zeros(nsamp, 1);

%    for the i'th time sample of data...     
     for i = 1:nsamp

%        read the data contained in the i'th sample
         [temp,count] = fread(fid, nchan, 'float32');
	
%        store the i'th data sample (consisting of all data
%        channels) in the i'th row of the data array
         data(i,:) = transpose(temp);

%        read the error byte in the i'th sample
         [temp,count] = fread(fid,1,'uint8');

%        store the i'th data sample (consisting of only the
%        error byte represented as a decimal number) in the
%        i'th row of the data array
         errdec(i) = temp;

%    terminate loop begun as for i = 1:nsamp
     end;

%    close the file opened previously
     fclose(fid);

%terminate the segment begun as "if (fid ~= -1)"
 end;
 
end
 
 