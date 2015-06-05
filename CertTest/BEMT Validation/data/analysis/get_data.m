% [data] = engload_new(filnam, nchan, nsamp)
clear; close all;

wind_speed = [5, 7, 10, 13, 15];
yaw = [0, 10, 20, 30, 45, 60, 75, 90, 135, 180];

wind_speed = [7];
yaw = [20];

hd_file = '';
eng_file = '';

for i = 1:length(wind_speed)
    for j = 1:length(yaw)
        % setup file names
        filenum = ['H', num2str(wind_speed(i), '%02d'), ...
            num2str(yaw(j), '%04d'), '0'];
        hd_file = [filenum, '.HD1'];
        eng_file = [filenum, '.ENG'];

        % get number of channels and samples
        fid = fopen(hd_file);
        nchan = fscanf(fid, '%*s %*s %*s\n %d', 1);
        nsamp = fscanf(fid, ',%d', 1);
        fclose(fid);

        % grab locations of specific data
        fid = fopen(hd_file);
        s = textscan(fid, '%s', 'delimiter', '\n');
        fclose(fid);

        span = [0.3, 0.47, 0.63, 0.8, 0.95];
        cth = zeros(1, 5);
        ctq = zeros(1, 5);
        cth(1) = get_mean(s, 'Thrust coefficient at 30% span');
        cth(2) = get_mean(s, 'Thrust coefficient at 47% span');
        cth(3) = get_mean(s, 'Thrust coefficient at 63% span');
        cth(4) = get_mean(s, 'Thrust coefficient at 80% span');
        cth(5) = get_mean(s, 'Thrust coefficient at 95% span');

        ctq(1) = get_mean(s, 'Torque coefficient at 30% span');
        ctq(2) = get_mean(s, 'Torque coefficient at 47% span');
        ctq(3) = get_mean(s, 'Torque coefficient at 63% span');
        ctq(4) = get_mean(s, 'Torque coefficient at 80% span');
        ctq(5) = get_mean(s, 'Torque coefficient at 95% span');

        cone = get_mean(s, 'Cone angle')*pi/180.0;

        Q = get_mean(s, 'LSSTQCOR');
        HB = get_mean(s, 'LSSHB');
        VB = get_mean(s, 'LSSVB');


        cth_idx = zeros(1, 5);
        % idx = strfind(s{1}, 'Thrust coefficient at 30% span');
        % cth_idx(1) = find(~cellfun(@isempty,idx)) - 2;
        % idx = strfind(s{1}, 'Thrust coefficient at 47% span');
        % cth_idx(2) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'Thrust coefficient at 63% span');
        cth_idx(3) = find(~cellfun(@isempty,idx)) - 2;
        % idx = strfind(s{1}, 'Thrust coefficient at 80% span');
        % cth_idx(4) = find(~cellfun(@isempty,idx)) - 2;
        % idx = strfind(s{1}, 'Thrust coefficient at 95% span');
        % cth_idx(5) = find(~cellfun(@isempty,idx)) - 2;

        [data] = engload_new(eng_file, nchan, nsamp);
     %   cth_idx(3)
     %   eng_file, nchan, nsamp
     %   data(1:100, cth_idx(3))

     %   exit


        % ctq_idx = zeros(1, 5);
        % idx = strfind(s{1}, 'Torque coefficient at 30% span');
        % ctq_idx(1) = find(~cellfun(@isempty,idx)) - 2;
        % idx = strfind(s{1}, 'Torque coefficient at 47% span');
        % ctq_idx(2) = find(~cellfun(@isempty,idx)) - 2;
        % idx = strfind(s{1}, 'Torque coefficient at 63% span');
        % ctq_idx(3) = find(~cellfun(@isempty,idx)) - 2;
        % idx = strfind(s{1}, 'Torque coefficient at 80% span');
        % ctq_idx(4) = find(~cellfun(@isempty,idx)) - 2;
        % idx = strfind(s{1}, 'Torque coefficient at 95% span');
        % ctq_idx(5) = find(~cellfun(@isempty,idx)) - 2;

        % idx = strfind(s{1}, 'Cone angle');
        % idx = find(~cellfun(@isempty,idx));
        % cone_string = s{1}{idx};
        % cone_split = strsplit(cone_string, ',');
        % cone = str2double(cone_split{6})*pi/180.0;

        idx = strfind(s{1}, 'Clock - second');
        s_idx = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'Clock - millisecond');
        ms_idx = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'Blade azimuth angle');
        baz_idx = find(~cellfun(@isempty,idx)) - 2;


        % load data
      %  [data] = engload_new(eng_file, nchan, nsamp);

%         % plot loading across span
%         span = [0.3, 0.47, 0.63, 0.8, 0.95];
%         cth = zeros(1, 5);
%         ctq = zeros(1, 5);
%         for iavg = 1:5
%             cth(iavg) = mean(data(:, cth_idx(iavg)));  % just straight time average
%             ctq(iavg) = mean(data(:, ctq_idx(iavg)));
%         end

      %  cth_idx(3)
      %  data(:, cth_idx(3))/cos(cone)
      %  data(:, baz_idx)
        % data(:, s_idx) + data(:, ms_idx)/1000.0


        % use my c.s. definition
        cx = cth/cos(cone);
        cy = -ctq/cos(cone);

        figure;
        plot(span, cx, 'o');
        xlim([0, 1]);
        y = ylim;
        ylim([0, y(2)]);
        xlabel('r/R');
        ylabel('c_x');

        figure;
        plot(span, cy, 'o');
        xlim([0, 1]);
        y = ylim;
        ylim([y(1), 0]);
        xlabel('r/R');
        ylabel('c_y');

       % Q
       % HB
       % VB

    end
end