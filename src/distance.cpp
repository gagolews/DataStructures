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


#include <algorithm>
#include "distance.h"
using namespace DataStructures;



void DistanceStats::print() const
{
#if VERBOSE > 0
#if defined(HASHMAP_ENABLED) && defined(GENERATE_STATS)
   Rprintf("             distance function hashmap #hits: %.0f, #miss: %.0f, est.mem.used: ~%.1fMB (vs %.1fMB)\n",
      (double)hashmapHit, (double)hashmapMiss,
      8.0f*hashmapMiss/1000.0f/1000.0f,
      8.0f*distCallTheoretical/1000.0f/1000.0f);
#endif
#if defined(GENERATE_STATS)
   Rprintf("             distance function total calls: %.0f (i.e., %.2f%% of %.0f)\n",
      (double)distCallCount,
      (double)distCallCount*100.0/(double)distCallTheoretical,
      (double)distCallTheoretical
   );
#if defined(MEASURE_MEM_USE)
   Rprintf("             currentRSS=%.0f MB, peakRSS=%.0f MB\n",
      (double)getCurrentRSS()/1000.0/1000.0,
      (double)getPeakRSS()/1000.0/1000.0
   );
#endif
#endif
#endif
}


Distance::Distance(size_t n) :
#ifdef HASHMAP_ENABLED
   hashmap(std::vector< std::unordered_map<size_t, double> >(n)),
#endif
   stats(DistanceStats(n)),
   n(n)
{
#ifdef HASHMAP_ENABLED
   MESSAGE_1("Warning: HASHMAP_ENABLED is defined in hclust2_distance.h\n");
#endif
#ifdef GENERATE_STATS
   MESSAGE_1("Warning: GENERATE_STATS is defined in hclust2_distance.h\n");
#endif
}


Distance::~Distance()
{
// #if VERBOSE > 5
//    Rprintf("[%010.3f] destroying distance object (base)\n", clock()/(float)CLOCKS_PER_SEC);
// #endif
}


#ifdef HASHMAP_ENABLED
double Distance::operator()(size_t v1, size_t v2)
{
   if (v1 == v2) return 0.0;
   if (v1 > v2) std::swap(v1, v2);

#ifdef GENERATE_STATS
#ifdef _OPENMP
#pragma omp atomic
#endif
      ++stats.distCallCount;
#endif

   // this is thread unsafe, but we use it only for testing:
   auto got = hashmap[v1].find(v2);
   if ( got == hashmap[v1].end() )
   {
#ifdef GENERATE_STATS
#ifdef _OPENMP
#pragma omp atomic
#endif
      ++stats.hashmapMiss;
#endif
      double d = compute(v1, v2);
      hashmap[v1].emplace(v2, d);
      return d;
   }
   else
   {
#ifdef GENERATE_STATS
#ifdef _OPENMP
#pragma omp atomic
#endif
      ++stats.hashmapHit;
#endif
      return got->second;
   }
}
#endif


Distance* Distance::createDistance(Rcpp::RObject distance, Rcpp::RObject objects)
{
   if (Rf_isVectorList(objects) && Rf_isFunction(distance))
   {
      Rcpp::Function distance2(distance);
      Rcpp::List objects2(objects);
      return (DataStructures::Distance*)
         new DataStructures::GenericRDistance(
            distance2,
            objects2
         );
   }
   else if (Rf_isNumeric(distance) && Rf_isObject(distance) && !strcmp(distance.attr("class"), "dist") && Rf_isNull(objects))
   {
      return (DataStructures::Distance*)
            new DataStructures::DistObjectDistance(
               (Rcpp::NumericVector)distance
            );
   }
   else if (Rf_isMatrix(objects) && Rf_isNumeric(objects) && (Rf_isNull(distance) || Rf_isString(distance)))
   {
      Rcpp::NumericMatrix objects2(objects);
      Rcpp::CharacterVector distance2 =
         ((Rf_isNull(distance))?Rcpp::CharacterVector("euclidean"):Rcpp::CharacterVector(distance));

      const char* distance3 = CHAR(STRING_ELT((SEXP)distance2, 0));
      if (!strcmp(distance3, "euclidean")) {
         return (DataStructures::Distance*)
            new DataStructures::EuclideanDistance(
               objects2
            );
      }
      else if (!strcmp(distance3, "manhattan")) {
         return (DataStructures::Distance*)
            new DataStructures::ManhattanDistance(
               objects2
            );
      }
      else if (!strcmp(distance3, "maximum")) {
         return (DataStructures::Distance*)
            new DataStructures::MaximumDistance(
               objects2
            );
      }
      else if (!strcmp(distance3, "hamming")) {
         return (DataStructures::Distance*)
            new DataStructures::HammingDistance(
               objects2
            );
      }
      else {
         Rcpp::stop("`distance` should be one of: \"euclidean\" (default), \"manhattan\", \"maximum\", \"hamming\"");
      }
   }
   else {
      Rcpp::stop("incorrect input data");
   }
   return NULL;
}



GenericMatrixDistance::GenericMatrixDistance(const Rcpp::NumericMatrix& points) :
      Distance(points.nrow()),
      items(REAL((SEXP)points)), m(points.ncol())  {
   // act on a transposed matrix to avoid many L1/L... cache misses
   items = new double[m*n];
   const double* items2 = REAL((SEXP)points);
   double* items_ptr = items;
   for (size_t i=0; i<n; ++i) {
      for (size_t j=0; j<m; ++j) {
         if (!std::isfinite(items2[j*n+i]))
            Rcpp::stop("missing values and infinities in input objects are not allowed");
         *(items_ptr++) = items2[j*n+i];
      }
   }
}


double EuclideanDistance::compute(size_t v1, size_t v2)
{
   if (v1 == v2) return 0.0;
   double d = 0.0;
   for (size_t i=0; i<m; ++i)
      d += (items[v1*m+i]-items[v2*m+i])*(items[v1*m+i]-items[v2*m+i]);
   return sqrt(d);

// this is not faster:
//    double d = sqobs[v1]+sqobs[v2]; // already multiplied by 0.5
//    // sum((x-y)^2) == 2*(sum(x^2)/2 + sum(y^2)/2 - sum(x*y))
//    const double* items1_ptr = items+v1*m;
//    const double* items2_ptr = items+v2*m;
//    for (size_t i=0; i<m; ++i)
//       d -= (*(items1_ptr++))*(*(items2_ptr++));
//    return sqrt(2.0*d);
}


double ManhattanDistance::compute(size_t v1, size_t v2)
{
   if (v1 == v2) return 0.0;
   double d = 0.0;
   for (size_t i=0; i<m; ++i)
      d += std::abs(items[v1*m+i]-items[v2*m+i]);
   return d;
}


double MaximumDistance::compute(size_t v1, size_t v2)
{
   if (v1 == v2) return 0.0;
   double d = 0.0;
   for (size_t i=0; i<m; ++i) {
      double d2 = std::abs(items[v1*m+i]-items[v2*m+i]);
      if (d2 > d) d = d2;
   }
   return d;
}

double HammingDistance::compute(size_t v1, size_t v2)
{
   if (v1 == v2) return 0.0;
   double d = 0.0;
   for (size_t i=0; i<m; ++i) {
      if (items[v1*m+i] != items[v2*m+i]) d += 1.0;
   }
   return d;
}


double GenericRDistance::compute(size_t v1, size_t v2)
{
   if (v1 == v2) return 0.0;
   return ((Rcpp::NumericVector)distfun(items[v1], items[v2]))[0];
}


double DistObjectDistance::compute(size_t v1, size_t v2)
{
   if (v1 == v2) return 0.0;

   size_t i;
   if (v1 < v2)
      i = n*v1-((v1+1)*(v1))/2+v2-v1-1;
   else
      i = n*v2-((v2+1)*(v2))/2+v1-v2-1;

   return items[i];
}
