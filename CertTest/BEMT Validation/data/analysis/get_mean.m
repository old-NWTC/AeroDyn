function [mean_val] = get_mean(s, text)

    cell_array = s{1};

    idx = strfind(cell_array, text);
    idx = find(~cellfun(@isempty,idx));
    val_split = strsplit(cell_array{idx}, ',');
    mean_val = str2double(val_split{6});