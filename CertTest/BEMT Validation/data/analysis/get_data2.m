% [data] = engload_new(filnam, nchan, nsamp)
clear; close all;
function [az_vec, th_vec_all, tq_vec_all] = get_uae_azimuth_data(wind_speed, yaw)


hd_file = '';
eng_file = '';


        % setup file names
        filenum = ['H', num2str(wind_speed, '%02d'), ...
            num2str(yaw, '%04d'), '0'];
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
        r_exp = [0.3, 0.47, 0.63, 0.8, 0.95];
        c_exp = [0.711, 0.62492758, 0.54337911, 0.457, 0.381];
        
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


        th_idx = zeros(1, 5);
        idx = strfind(s{1}, 'Thrust coefficient at 30% span');
        th_idx(1) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'Thrust coefficient at 47% span');
        th_idx(2) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'Thrust coefficient at 63% span');
        th_idx(3) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'Thrust coefficient at 80% span');
        th_idx(4) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'Thrust coefficient at 95% span');
        th_idx(5) = find(~cellfun(@isempty,idx)) - 2;

        
        q_idx = zeros(1, 5);
        idx = strfind(s{1}, 'QNORM30');
        q_idx(1) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'QNORM47');
        q_idx(2) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'QNORM63');
        q_idx(3) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'QNORM80');
        q_idx(4) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'QNORM95');
        q_idx(5) = find(~cellfun(@isempty,idx)) - 2;
        
        
        tq_idx = zeros(1, 5);
        idx = strfind(s{1}, 'Torque coefficient at 30% span');
        tq_idx(1) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'Torque coefficient at 47% span');
        tq_idx(2) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'Torque coefficient at 63% span');
        tq_idx(3) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'Torque coefficient at 80% span');
        tq_idx(4) = find(~cellfun(@isempty,idx)) - 2;
        idx = strfind(s{1}, 'Torque coefficient at 95% span');
        tq_idx(5) = find(~cellfun(@isempty,idx)) - 2;

        idx = strfind(s{1}, 'CONE');
        cone_idx = find(~cellfun(@isempty,idx)) - 2;
        
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

        
        d2r = pi/180;
        r2d = 180/pi;
        
        [data] = engload_new(eng_file, nchan, nsamp);
        azimuth = 0;
        az_vec   = data(:,baz_idx); 
        idx = find(abs(az_vec - azimuth) < 1);
        cone_vec = data(:, cone_idx);
        station_idx = 1;
        
        for i=1:5
            cth_vec = data(:, th_idx(i));
            ctq_vec = data(:, tq_idx(i));
            q_vec   = data(:, q_idx(i));

            th_vec = cth_vec .* q_vec .* c_exp(i) ./ cos(d2r.*cone_vec);
            tq_vec = ctq_vec .* q_vec .* c_exp(i) ./ cos(d2r.*cone_vec);

            th_mean(i) = mean(th_vec(idx));
            th_max(i) = max(th_vec(idx));
            th_min(i) = min(th_vec(idx));

            tq_mean(i) = mean(tq_vec(idx));
            tq_max(i) = max(tq_vec(idx));
            tq_min(i) = min(tq_vec(idx));

            th_vec_all(i, :) = th_vec;
            tq_vec_all(i, :) = tq_vec;
        end
        
        figure;
        plot(az_vec, th_vec_all(station_idx, :), '.');
        
        
    

end