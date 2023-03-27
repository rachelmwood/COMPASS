#include <iostream>
#include <unistd.h>

#ifdef _OPENMP
    #include <omp.h>
#else
    #define omp_get_thread_num() 0
#endif

void times_table(int n)
{
    int thread_id = omp_get_thread_num();

    for (int i=1; i<=n; ++i)
    {
        int i_times_n = i * n;
        std::cout << "Thread " << thread_id << " says " << i
                  << " times " << n << " equals " << i_times_n << std::endl;
        sleep(1);
    }
}

void countdown()
{
    int thread_id = omp_get_thread_num();

    for (int i=10; i>=1; --i)
    {
        std::cout << "Thread " << thread_id << " says " << i << "...\n";
        sleep(1);
    }

    std::cout << "Thread " << thread_id << " says \"Lift off!\"\n";
}

void long_loop()
{
    double sum = 0;

    int thread_id = omp_get_thread_num();

    for (int i=1; i<=10; ++i)
    {
        sum += (i*i);
        sleep(1);
    }

    std::cout << "Thread " << thread_id << " says the sum of the long loop is "
              << sum << std::endl;
}

int main(int argc, const char **argv)
{
    std::cout << "This is the main thread.\n";

    #pragma omp parallel
    {
        #pragma omp sections
        {
            #pragma omp section
            {
                times_table(12);
            }
            #pragma omp section
            {
                countdown();
            }
            #pragma omp section
            {
                long_loop();
            }
        }
    }

    std::cout << "Back to the main thread. Goodbye!\n";

    return 0;
}
