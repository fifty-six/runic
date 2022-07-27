struct S0 {
  int A;
  int B;
};

struct S1 : S0 {
  int C;
};

int main() {
  S0 a {};
  S1 b {};

  b.A = 3;
  b.C = 4;

  return 0;
}
