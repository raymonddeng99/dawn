pub struct EulerTourTree<T> {
    pub tour: Vec<T>,
    pub first: Vec<usize>,
    pub last: Vec<usize>,
    pub level: Vec<usize>,
}

fn build_euler_tour<T: Clone>(root: &Option<Box<Node<T>>>) -> Vec<T> {
    let mut tour = Vec::new();
    build_euler_tour_helper(root, &mut tour);
    tour
}

fn build_euler_tour_helper<T: Clone>(node: &Option<Box<Node<T>>>, tour: &mut Vec<T>) {
    if let Some(node) = node {
        tour.push(node.val.clone());
        for child in &node.children {
            build_euler_tour_helper(&Some(child.clone()), tour);
        }
        tour.push(node.val.clone());
    }
}

pub fn build_euler_tour_tree<T: Clone + Eq>(tour: &[T]) -> EulerTourTree<T> {
    let n = tour.len();
    let mut first = vec![0; n];
    let mut last = vec![0; n];
    let mut level = vec![0; n];
    let mut stack = Vec::new();
    for (i, &val) in tour.iter().enumerate() {
        if stack.is_empty() {
            stack.push(i);
            level[i] = 0;
        } else {
            let j = *stack.last().unwrap();
            if val == tour[j] {
                stack.pop();
                last[j] = i;
            } else {
                stack.push(i);
                level[i] = level[j] + 1;
                first[i] = j;
            }
        }
    }
    EulerTourTree {
        tour: tour.to_vec(),
        first,
        last,
        level,
    }
}