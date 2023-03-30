#include <iostream>
#include <vector>

int main()
{
    std::vector< std::vector<int> > m;

    // create a 3x3 matrix
    for (int i=1; i<=3; ++i)
    {
        //create space for a row
        std::vector<int> row;

        for (int j=1; j<=3; ++j)
        {
            row.push_back( i * j );
        }

        //now save the row in the matrix
        m.push_back(row);
    }

    //loop over elements of m (columns)
    for (auto x : m)
    {
        //loop other elements of the columns
        for (auto y : x){
	          std::cout << y << " ";
	      }
	      std::cout << std::endl;
    }
    return 0;
}
