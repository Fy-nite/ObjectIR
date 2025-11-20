int add(int a, int b) {
    int result = 0;
    result = a + b;
    return result;
}

int multiply(int x, int y) {
    return x * y;
}

char* greet(const char* name) {
    return "Hello";
}

int main() {
    int x = 5;
    int y = 3;
    int sum = 0;
    sum = add(x, y);
    int product = multiply(sum, y);
    char* message = greet("World");
    return product;
}