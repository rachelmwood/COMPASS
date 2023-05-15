#include "filecounter.h"
#include <iostream>
#include <vector>
#include <tuple>
#include <cmath>
#include <functional>
#include <string>
#include <fstream>
#include <random>

using namespace filecounter;

namespace detail
{
    template<class ARG>
    size_t get_min_container_size(const std::vector<ARG> &arg)
    {
        return arg.size();
    }

    template<class ARG1, class ARG2>
    size_t get_min_container_size(const std::vector<ARG1> &arg1, const std::vector<ARG2> &arg2)
    {
        return std::min(arg1.size(), arg2.size());
    }

    template<class ARG1, class ARG2, class... ARGS>
    size_t get_min_container_size(const std::vector<ARG1> &arg1, const std::vector<ARG2> &arg2,
                                  const std::vector<ARGS>&... args)
    {
        size_t minsize = get_min_container_size(args...);
        return std::min( minsize, get_min_container_size(arg1,arg2) );
    }

    std::default_random_engine generator;
    std::uniform_real_distribution<double> distribution(0.0,1.0);
}

template<class FUNC, class... ARGS>
auto map(FUNC func, const std::vector<ARGS>&... args)
{
    typedef typename std::result_of<FUNC(ARGS...)>::type RETURN_TYPE;

    int nargs=detail::get_min_container_size(args...);

    std::vector<RETURN_TYPE> result(nargs);

    for (size_t i=0; i<nargs; ++i)
    {
        result[i] = func(args[i]...);
    }

    return result;
}

int main(int argc, char **argv)
{
    	
    auto filenames = get_arguments(argc, argv);

    auto results = map( count_lines, filenames );

    for (size_t i=0; i<filenames.size(); ++i)
    {
        std::cout << filenames[i] << " = " << results[i] << std::endl;
    }

    
    return 0;
}

