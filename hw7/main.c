#include <stdio.h>

int match(const char* cs);

int main(int argc, char** argv)
{
    int i;

    for (i = 1; i < argc; ++i) {
        printf("\"%s\" %s\n",
               argv[i],
               match(argv[i]) ? "matched" : "did not match");
    }
    return 0;
}
