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



#ifndef __DISTANCE_H
#define __DISTANCE_H

#include <boost/functional/hash.hpp>
#include <vector>
#include <limits>
#include <numeric>
#include <unordered_map>
#include <Rcpp.h>


namespace DataStructures
{

struct DistanceStats
{
   size_t hashmapHit;
   size_t hashmapMiss;
   size_t distCallCount;
   size_t distCallTheoretical;

   DistanceStats(size_t n) :
      hashmapHit(0), hashmapMiss(0), distCallCount(0),
      distCallTheoretical(n*(n-1)/2) {}

   void print() const;

   Rcpp::NumericVector toR() const {
      return Rcpp::NumericVector::create(
         Rcpp::_["hashmapHit"]
            = (hashmapHit>0)?(double)hashmapHit:NA_REAL,
         Rcpp::_["hashmapMiss"]
            = (hashmapMiss>0)?(double)hashmapMiss:NA_REAL,
         Rcpp::_["distCallCount"]
            = (distCallCount>0)?(double)distCallCount:NA_REAL,
         Rcpp::_["distCallTheoretical"]
            = (distCallTheoretical>0)?(double)distCallTheoretical:NA_REAL
      );
   }
};


class Distance
{
private:
#ifdef HASHMAP_ENABLED
   std::vector< std::unordered_map<size_t, double> > hashmap;
#endif
   DistanceStats stats;

protected:
   size_t n;
   virtual double compute(size_t v1, size_t v2) = 0;

public:
   Distance(size_t n);
   virtual ~Distance();
   inline size_t getObjectCount() { return n; }
   static Distance* createDistance(Rcpp::RObject distance, Rcpp::RObject objects);

   virtual Rcpp::RObject getLabels() { /* stub */ return R_NilValue; }
   virtual Rcpp::RObject getDistMethod() { /* stub */ return R_NilValue; }

   inline const DistanceStats& getStats() { return stats; }

#ifdef HASHMAP_ENABLED
   double operator()(size_t v1, size_t v2);
#else
   inline double operator()(size_t v1, size_t v2) {
#ifdef GENERATE_STATS
   #ifdef _OPENMP
      #pragma omp atomic
   #endif
      ++stats.distCallCount;
#endif
      return compute(v1, v2);
   }
#endif
};


class GenericMatrixDistance : public Distance
{
protected:
   double* items;
   size_t m;

public:
   // TO DO: virtual Rcpp::RObject getLabels() { /* stub */ return R_NilValue; } --- get row names

   GenericMatrixDistance(const Rcpp::NumericMatrix& points);

   virtual ~GenericMatrixDistance() {
// #if VERBOSE > 5
//       Rprintf("[%010.3f] destroying distance object\n", clock()/(float)CLOCKS_PER_SEC);
// #endif
      delete [] items;
   }
};


class EuclideanDistance : public GenericMatrixDistance
{
// private:
//    std::vector<double> sqobs;

protected:
   virtual double compute(size_t v1, size_t v2);

public:
   virtual Rcpp::RObject getDistMethod() { return Rf_mkString("euclidean"); }

   EuclideanDistance(const Rcpp::NumericMatrix& points) :
      GenericMatrixDistance(points) {
//       const double* items_ptr = items;
//       for (size_t i=0; i<n; ++i) {
//          double sqobs_cur = 0.0;
//          for (size_t j=0; j<m; ++j) {
//             sqobs_cur += (*items_ptr)*(*items_ptr);
//             ++items_ptr;
//          }
//          sqobs[i] = sqobs_cur*0.5;
//       }
   }
};


class ManhattanDistance : public GenericMatrixDistance
{
protected:
   virtual double compute(size_t v1, size_t v2);

public:
   virtual Rcpp::RObject getDistMethod() { return Rf_mkString("manhattan"); }

   ManhattanDistance(const Rcpp::NumericMatrix& points) :
      GenericMatrixDistance(points)  {   }
};


class MaximumDistance : public GenericMatrixDistance
{
protected:
   virtual double compute(size_t v1, size_t v2);

public:
   virtual Rcpp::RObject getDistMethod() { return Rf_mkString("maximum"); }

   MaximumDistance(const Rcpp::NumericMatrix& points) :
      GenericMatrixDistance(points)  {   }
};


class HammingDistance : public GenericMatrixDistance
{
protected:
   virtual double compute(size_t v1, size_t v2);

public:
   virtual Rcpp::RObject getDistMethod() { return Rf_mkString("hamming"); }

   HammingDistance(const Rcpp::NumericMatrix& points) :
      GenericMatrixDistance(points)  {   }
};


class GenericRDistance : public Distance
{
private:
   Rcpp::Function distfun;
   Rcpp::List items;

protected:
   virtual double compute(size_t v1, size_t v2);

public:
   //virtual Rcpp::RObject getDistMethod() { return Rf_mkString("euclidean"); } ....deparse???? in R
   // virtual Rcpp::RObject getDistMethod() { return Rcpp::RObject(robj1).attr("names"); } .... get names attrib from items....

   GenericRDistance(const Rcpp::Function& distfun, const Rcpp::List& items) :
         Distance(items.size()),
         distfun(distfun),
         items(items) {
      R_PreserveObject(distfun);
      R_PreserveObject(items);
   }

   virtual ~GenericRDistance() {
      R_ReleaseObject(distfun);
      R_ReleaseObject(items);
   }
};


class DistObjectDistance : public Distance
{
protected:
   SEXP robj1;
   const double* items;

protected:
   virtual double compute(size_t v1, size_t v2);

public:
   virtual Rcpp::RObject getLabels() {  return Rcpp::RObject(robj1).attr("Labels"); }
   virtual Rcpp::RObject getDistMethod() { return Rcpp::RObject(robj1).attr("method"); }

   DistObjectDistance(const Rcpp::NumericVector& distobj) :
         Distance((size_t)((Rcpp::NumericVector)distobj.attr("Size"))[0]),
         robj1(distobj),
         items(REAL((SEXP)distobj)) {
      if ((size_t)XLENGTH((SEXP)distobj) != n*(n-1)/2)
         Rcpp::stop("incorrect dist object length.");
      R_PreserveObject(robj1);
   }

   virtual ~DistObjectDistance()  {
      R_ReleaseObject(robj1);
   }
};

} // namespace DataStructures

#endif
