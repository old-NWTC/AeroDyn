function StripFASTtext(FileName, delim, HeaderRows, NameLine, UnitsLine, yaw )
%[Channels, ChanName, ChanUnit] = ReadFASTtext(FileName)
% Author: Bonnie Jonkman, National Renewable Energy Laboratory
% (c) 2012, National Renewable Energy Laboratory
%
%  Edited for FAST v7.02.00b-bjj  22-Oct-2012
%
% Input:
%  FileName      - string: contains file name to open
% Optional Input:
%    delim      - the column delimiter, default is all whitespace
%    HeaderRows - the number of header rows in the file; default is 8
%    NameLine   - the line number containing the column names; default is
%                 max( HeaderRows - 1, 0);
%    UnitsLine  - the line number containing the column units; default is
%                 min( NameLine + 1, HeaderRows );
%
% Output:
%  Channels      - 2-D array: dimension 1 is time, dimension 2 is channel 
%  ChanName      - cell array containing names of output channels
%  ChanUnit      - cell array containing unit names of output channels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
LenName = 10;  % number of characters per channel name
LenUnit = 10;  % number of characters per unit name
DescStr = '';

nrev = 160;
four_rev = nrev*4;
five_rev = nrev*5;
fourteen_rev = nrev*14;
fiveteen_rev = nrev*15;
twentyfour_rev = nrev*24;
twentyfive_rev = nrev*25;
switch nargin;
    case 1
        delim = '';
        HeaderRows = 8;
        NameLine   = 7;
        UnitsLine  = 8;
    case 2
        HeaderRows = 8;
        NameLine   = 7;
        UnitsLine  = 8;
    case 3
        NameLine   = max(HeaderRows - 1, 0);
        UnitsLine  = NameLine + 1;
    case 4
        UnitsLine  = NameLine + 1;
    case 5
        yaw = 0.0
    case 6
    otherwise
        error('ReadFASTtext::Invalid number of inputs.')
end
              
if nargout < 3
    UnitsLine = 0;
    if nargout < 2
        NameLine = 0;
    end
end
    
if UnitsLine > HeaderRows
    UnitsLine = 0;
end

if NameLine > HeaderRows
    NameLine = 0;
end
    
     
fid = fopen(FileName);
fidOut5 = fopen(['ADv14_' num2str(yaw) '.0Yaw_5_mps'],'w+t','n');
fidOut7 = fopen(['ADv14_' num2str(yaw) '.0Yaw_7_mps'],'w+t','n');
fidOut10 = fopen(['ADv14_' num2str(yaw) '.0Yaw_10_mps'],'w+t','n');
if fid <= 0
    disp(['ReadFASTtext::Error ' int2str(fid) ' reading from file, "' FileName '"'] )
    Channels     = [];
    ChanName = {};
    ChanUnit = {};
else    
    nCols1 = 0;
    nCols2 = 0;
    
    
        for i = 1:HeaderRows
            line = fgetl(fid);
            fprintf(fidOut5,[line '\n']);
            fprintf(fidOut7,[line '\n']);
            fprintf(fidOut10,[line '\n']);
        end %for i
            
      

        % read the data and throw out unwanted data
        for i=1:twentyfive_rev
            line = fgetl(fid);
            if ((i > four_rev) && (i <= five_rev))
               fprintf(fidOut5,[line '\n']);
            end
            if ((i > fourteen_rev) && (i <= fiveteen_rev))
               fprintf(fidOut7,[line '\n']);
            end
            if ((i > twentyfour_rev) && (i <= twentyfive_rev))
               fprintf(fidOut10,[line '\n']);
            end
        end
        
        fclose(fid);
        fclose(fidOut5);
        fclose(fidOut7);
        fclose(fidOut10);

    

   

end


return;

