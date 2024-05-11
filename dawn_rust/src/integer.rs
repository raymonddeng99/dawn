pub fn signature_sort<T: Ord + Copy>(arr: &mut [T]) {
    merge_sort(arr, 0, arr.len());
}

fn merge_sort<T: Ord + Copy>(arr: &mut [T], start: usize, end: usize) {
    if end - start < 2 {
        return;
    }

    let mid = start + (end - start) / 2;
    merge_sort(arr, start, mid);
    merge_sort(arr, mid, end);
    merge(arr, start, mid, end);
}

fn merge<T: Ord + Copy>(arr: &mut [T], start: usize, mid: usize, end: usize) {
    let mut i = start;
    let mut j = mid;
    let mut temp = Vec::with_capacity(end - start);

    while i < mid && j < end {
        if arr[i] <= arr[j] {
            temp.push(arr[i]);
            i += 1;
        } else {
            temp.push(arr[j]);
            j += 1;
        }
    }

    while i < mid {
        temp.push(arr[i]);
        i += 1;
    }

    while j < end {
        temp.push(arr[j]);
        j += 1;
    }

    for (i, val) in temp.iter().enumerate() {
        arr[start + i] = *val;
    }
}