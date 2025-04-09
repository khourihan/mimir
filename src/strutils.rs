pub fn unquote(s: &mut String) {
    assert!(s.len() >= 2);
    s.pop();
    s.remove(0);
}

pub fn bracket(s: &mut String, left: &str, right: &str) {
    s.insert_str(0, left);
    s.push_str(right);
}
