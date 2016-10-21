
function PlotAxIndTnIndCurves_DT(Index, baseName, baseFolder )

if (nargin < 1)
Index = 1;  % Valid values are 1, 2, 3
end 

if (nargin < 2)
   %baseName = 'AOC';
   baseName = 'NRELOffshrBsline5MW_Onshore';
end
if (nargin < 3)
   baseFolder = '.\';
  % baseFolder = '.\NREL_Results';
   %baseFolder = '.\Results';
end

LineColors     = {[0 0 0],[0 1 1],[1 0 1],[0 1 0],[0 0 1],[1 0 0]};
Markers        = {'o'    ,'s'    ,'d'    ,'v'    ,'^'    ,'.'    };
delim='';
HeaderRows= 8;
NameLine= 7;
UnitsLine= 8;
%legendStr = {'0.000138 sec', '0.00138 sec', '0.0138 sec','0.1.38 sec'};
legendStr = {'0.00138 sec', '0.0138 sec','0.1.38 sec'};
   % Loop over different timestep sizes
for iCase=3:-1:1
   FileName = [baseFolder '\' baseName '_Test0' num2str(Index) '.' num2str(iCase) '.out'];
   [Channels, ChanName, ChanUnit,DescStr] = ReadFASTtext(FileName, delim, HeaderRows, NameLine, UnitsLine );
   
   if ( iCase == 3 ) 
      AoACols = find(cellfun('length',regexp(ChanName,'Alpha')) == 1);
      AxIndCols = find(cellfun('length',regexp(ChanName,'AxInd')) == 1);
      TnIndCols  = find(cellfun('length',regexp(ChanName,'TnInd')) == 1);
      numNodes = length(AoACols); 
   end
         %numSteps = size(Channels,1);
         %chanPerNode = size(Channels,2)/numNodes;
         %AoAData = zeros(numSteps, numNodes, numCases);
         %titleText = DescStr;
   for iNode = 2:numNodes   
      if ( iCase == 3 )
         
         AxIndFigs(iNode) = figure();
         grid;
         ylabel('AxInd');    
         %xlabel('AOA');
         xlabel('time');
         title(['Node Output #' num2str(iNode)]);
         
         TnIndFigs(iNode) = figure();
         grid;
         ylabel('TnInd');    
         %xlabel('AOA');
         xlabel('time');
         title(['Node Output #' num2str(iNode)]);
         
         
         
      end
      figure(AxIndFigs(iNode));
      hold on;
      %plot(Channels(5:end,AoACols(iNode)),Channels(5:end,AxIndCols(iNode)),'Color',LineColors{iCase},'LineWidth',2);
      plot(Channels(5:end,1),Channels(5:end,AxIndCols(iNode)),'Color',LineColors{iCase},'LineWidth',2);
      figure(TnIndFigs(iNode));
      hold on;
      %plot(Channels(5:end,AoACols(iNode)),Channels(5:end,TnIndCols(iNode)),'Color',LineColors{iCase},'LineWidth',2);
      plot(Channels(5:end,1),Channels(5:end,TnIndCols(iNode)),'Color',LineColors{iCase},'LineWidth',2);
      
  
   end
end
 % Legends
 for iNode = 2:numNodes  
    figure(AxIndFigs(iNode));
    legend(legendStr);
    figure(TnIndFigs(iNode));
    legend(legendStr);
    
 end
end

