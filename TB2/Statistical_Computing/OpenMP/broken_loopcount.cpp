#include <iostream>

#ifdef _OPENMP
    #include <omp.h>
#else
    #define omp_get_thread_num() 0
#endif

int main(int argc, char **argv)
{
    int global_nloops = 0;

    #pragma omp parallel
    {
        int private_nloops = 0;

        int thread_id = omp_get_thread_num();

        #pragma omp for
        for (int i=0; i<100000; ++i)
        {
            ++private_nloops;
        }

        std::cout << "Thread " << thread_id << " adding its iterations ("
                  << private_nloops << ") to the sum (" << global_nloops 
                  << ")...\n";

        global_nloops += private_nloops;

        std::cout << "...total nloops now equals " << global_nloops << ".\n";
    }

    std::cout << "The total number of loop iterations is " << global_nloops 
              << ".\n";

    return 0;
}
