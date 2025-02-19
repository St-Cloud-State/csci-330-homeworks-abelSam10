#include <iostream>
#include <stack>
#include <utility> // for std::pair

using namespace std;


void swap(int &a, int &b) {
    int temp = a;
    a = b;
    b = temp;
}

// Partition function (using the last element as pivot)
int partition(int arr[], int low, int high) {
    int pivot = arr[high];  // choose pivot
    int i = low - 1;        // index of smaller element

    // Rearranging elements based on the pivot value
    for (int j = low; j < high; j++) {
        if (arr[j] < pivot) {
            i++;
            swap(arr[i], arr[j]);
        }
    }
    swap(arr[i + 1], arr[high]);  // place pivot at the correct position
    return i + 1;
}

// Iterative implementation of Quicksort using a stack
void quickSort(int arr[], int low, int high) {
    // Create an STL stack to store pairs of indices (low, high)
    stack<pair<int, int>> stackIndices;
    
    // Push the initial array indices onto the stack
    stackIndices.push({low, high});
    
    // Process the stack until it is empty
    while (!stackIndices.empty()) {
        // Retrieve and remove the top pair from the stack
        pair<int, int> current = stackIndices.top();
        stackIndices.pop();
        low = current.first;
        high = current.second;
        
        // Partition the current subarray if it has more than one element
        if (low < high) {
            int pivotIndex = partition(arr, low, high);
            
            // If there are elements on the left side of the pivot,
            // push that subarray onto the stack for later processing.
            if (pivotIndex - 1 > low)
                stackIndices.push({low, pivotIndex - 1});
            
            // Similarly, if there are elements on the right side of the pivot,
            // push that subarray onto the stack.
            if (pivotIndex + 1 < high)
                stackIndices.push({pivotIndex + 1, high});
        }
    }
}

// A simple driver program to test the iterative Quicksort
int main() {
    int arr[] = {17, 12, 4, 62, 9, 8, 13, 2};
    int n = sizeof(arr) / sizeof(arr[0]);

    cout << "Original array: ";
    for (int i = 0; i < n; i++)
        cout << arr[i] << " ";
    cout << endl;

    // Call quickSort on the entire array
    quickSort(arr, 0, n - 1);

    cout << "Sorted array:   ";
    for (int i = 0; i < n; i++)
        cout << arr[i] << " ";
    cout << endl;
    return 0;
}
