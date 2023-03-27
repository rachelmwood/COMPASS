#include <cmath>
#include <random>
#include <iostream>

int main()
{
    int n_inside = 0;
    int n_outside = 0;

    std::random_device rd;

    #pragma omp parallel reduction(+ : n_inside, n_outside)
    {
        int count_in = 0;
        int count_out = 0;

        std::minstd_rand generator(rd());
        std::uniform_real_distribution<> random(-1.0, 1.0);

        #pragma omp for
        for (int i=0; i<1000000; ++i)
        {
            double x = random(generator);
            double y = random(generator);

            double r = std::sqrt( x*x + y*y );

            if (r < 1.0)
            {
                ++count_in;
            }
            else
            {
                ++count_out;
            }
        }

        n_inside += count_in;
        n_outside += count_out;
    }

    double pi = (4.0 * n_inside) / (n_inside + n_outside);

    std::cout << "pi is approximately " << pi << std::endl;

    return 0;
}
