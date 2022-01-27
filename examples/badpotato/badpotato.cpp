#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

long long n, t, t2;
vector<int> s;

int main(void)
{
    cin >> n;
    for (long long i = 0; i < n; i++) {
        cin >> t;
        if (t != -1) {
            t2 += t;
        } else {
            s.push_back(t2);
            t2 = 0;
        }
        s.push_back(t2);
    }

    cout << *max_element(s.begin(), s.end()) << endl;
}
