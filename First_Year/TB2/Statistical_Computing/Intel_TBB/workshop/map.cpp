=#include <iostream>
#include <vector>

int sum(int x, int y)
{
    return x+y;
}

template<class T>
void print_vector(const std::vector<T> &values)
{
    std::cout << "[";

    for (const T &value : values)
    {
	std::cout << " " << value;
    }

    std::cout << " ]" << std::endl;
}

int main(int argc, char **argv)
{
    auto a = std::vector<int>( { 1, 2, 3, 4, 5 } );
    auto b = std::vector<int>( { 6, 7, 8, 9, 10 } );

    auto result = std::vector<int>( a.size() );

    for (int i=0; i < a.size(); i++)
    {
	result[i] = sum(a[i], b[i] );
    }

    print_vector(result);

    return 0;
}
