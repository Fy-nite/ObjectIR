// Example demonstrating string and I/O support
char* getMessage() {
    return "Hello from OIC!";
}

int getAnswer() {
    return 42;
}

int main() {
    char* message = getMessage();
    int answer = getAnswer();
    return answer;
}
