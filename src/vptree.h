/* ************************************************************************* *
 *   This file is part of the `DataStructures` package.                      *
 *                                                                           *
 *   Copyright 2015 Maciej Bartoszuk, Marek Gagolewski,                      *
 *                                                                           *
 *   'DataStructures' is free software: you can redistribute it and/or       *
 *   modify it under the terms of the GNU Lesser General Public License      *
 *   as published by the Free Software Foundation, either version 3          *
 *   of the License, or (at your option) any later version.                  *
 *                                                                           *
 *   'DataStructures' is distributed in the hope that it will be useful,     *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of          *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the            *
 *   GNU Lesser General Public License for more details.                     *
 *                                                                           *
 *   You should have received a copy of the GNU Lesser General Public        *
 *   License along with 'DataStructures'.                                    *
 *   If not, see <http://www.gnu.org/licenses/>.                             *
 * ************************************************************************* */

#ifndef __VPTREE_H
#define __VPTREE_H



// ************************************************************************


#include <Rcpp.h>
#include <R.h>
#include <Rmath.h>
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics.hpp>
// #include <fstream>
// #include <deque>
// #include <exception>
// #include <string>
// #include <boost/property_map/property_map.hpp>
// #include <boost/tuple/tuple_comparison.hpp>
// #include <algorithm>
#include <queue>
#include <vector>
#include "distance.h"

using namespace std;
using namespace Rcpp;

namespace DataStructures
{

struct HeapNeighborItem
{
   size_t index;
   double dist;

   HeapNeighborItem(size_t index, double dist) :
      index(index), dist(dist) {}

   HeapNeighborItem() :
      index(SIZE_MAX), dist(-INFINITY) {}

   inline bool operator<( const HeapNeighborItem& o ) const {
      return dist < o.dist;
   }
};


struct DistanceComparator
{
   size_t index;
   Distance* distance;

   DistanceComparator(size_t index, Distance* distance)
      : index(index), distance(distance) {}

   inline bool operator()(size_t a, size_t b) {
      return (*distance)( index, a ) < (*distance)( index, b );
   }
};


struct DistanceComparatorCached
{
   std::vector<double>* distances;

   DistanceComparatorCached(std::vector<double>* distances)
      : distances(distances) {}

   inline bool operator()(size_t a, size_t b) {
      return (*distances)[a] < (*distances)[b];
   }
};


struct VpTreeNode
{
   size_t vpindex;
   size_t left;
   size_t right;
   double radius;
   VpTreeNode* childL;
   VpTreeNode* childR;

   VpTreeNode() :
         vpindex(SIZE_MAX), left(SIZE_MAX), right(SIZE_MAX), radius(-INFINITY),
         childL(NULL), childR(NULL)  { }

   VpTreeNode(size_t left, size_t right) :
         vpindex(SIZE_MAX), left(left), right(right), radius(-INFINITY),
         childL(NULL), childR(NULL)  { }

   VpTreeNode(size_t vpindex, size_t left, size_t right, double radius) :
         vpindex(vpindex), left(left), right(right), radius(radius),
         childL(NULL), childR(NULL)  { }

   ~VpTreeNode() {
      if (childL) delete childL;
      if (childR) delete childR;
   }
};


class VpTree
{
protected:

   VpTreeNode* _root;
   size_t _n;
   Distance* _distance;
   std::vector<size_t> _indices;
   std::vector<size_t> _indicesinv;

   std::vector<double> distances;

   size_t chooseNewVantagePoint(size_t left, size_t right);
   VpTreeNode* buildFromPoints(size_t left, size_t right);

   void getNearestNeighborsFromMinRadiusRecursive(
      VpTreeNode* node, size_t index, double minR, double& maxR,
      std::priority_queue<HeapNeighborItem>& nnheap, size_t maxNNPrefetch);

public:

   VpTree(Distance* dist);
   ~VpTree();

   vector<HeapNeighborItem> getNearestNeighbors(size_t index, int k, double minR=-INFINITY, double maxR=INFINITY);

}; // class

} // namespace DataStructures


#endif
