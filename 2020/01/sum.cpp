#include <bits/stdc++.h>
using namespace std;

const int TARGET = 2020;

template<class Iter>
unordered_set<int> diff_with(Iter a, Iter b, int k) {
	unordered_set<int> s;
	for (; a != b; a++) {
		s.insert(k-*a);
	}
	return s;
}

int sum_of_two(vector<int> v, int k) {
	auto s = diff_with(v.begin(), v.end(), k);
	for (auto y : v) {
		auto it = s.find(y);
		if (it != s.end()) {
			int x = -(*it - k);
			return x*y;
		}
	}
	return -1;
}

int sum_of_three(vector<int> A, int k) {
	auto D = diff_with(A.begin(), A.end(), k);
	for (auto a : A) {
		unordered_set<int> DD;
		for (auto d : D) {
			DD.insert(d - a);
		}
		for (auto b : A) {
			auto it = DD.find(b);
			if (it != DD.end()) {
				return a * b * (k - a - b);
			}
		}
	}
	return -1;
}

int main() {
	ios_base::sync_with_stdio(0);
	cin.tie(0);

	vector<int> v;
	int x;
	while (cin >> x) {
		v.push_back(x);
	}
	cout << "Sum of two: " << sum_of_two(v, TARGET) << "\n";
	cout << "Sum of three: " << sum_of_three(v, TARGET) << "\n";
}
