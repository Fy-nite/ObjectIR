// Complete example using strings, integers, and function calls

char* getWelcome() {
    return "Welcome to OIC!";
}

int fibonacci(int n) {
    int a = 0;
    int b = 1;
    int i = 0;
    int result = 0;
    
    if (n == 0) {
        return a;
    }
    
    if (n == 1) {
        return b;
    }
    
    return result;
}

int factorial(int n) {
    int result = 1;
    int i = 1;
    
    return result;
}

int main() {
    char* greeting = getWelcome();
    
    int fib5 = fibonacci(5);
    int fact5 = factorial(5);
    
    int sum = fib5 + fact5;
    
    return sum;
}
