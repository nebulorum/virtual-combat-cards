/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
function FetchCtrl($scope, $http) {
    $scope.method = 'GET';
    $scope.url = 'player-view';
    $scope.running = false;
    $scope.hasData = false;

    $scope.fetchInitial = function () {
        $http({method: $scope.method, url: $scope.url + "?now=true", cache: false}).
            success(function (data, status) {
                $scope.status = status;
                $scope.updateState(data);
                $scope.fetch();
            }).
            error(function (data, status) {
                $scope.stopUpdating(status);
            });
    };

    $scope.updateState = function (data) {
        try {
            var dataRead = angular.fromJson(data);
        } catch (err) {
            console.log(err);
        }
        if (dataRead["state"]) {
            $scope.data = dataRead["state"];
            $scope.hasData = (typeof $scope.data === "object" && $scope.data.length > 0);
        }
    };

    $scope.fetch = function () {
        $scope.code = null;
        $scope.response = null;
        $scope.running = true;

        $http({method: $scope.method, url: $scope.url, cache: false}).
            success(function (data, status) {
                $scope.status = status;
                $scope.running = false;
                $scope.updateState(data);
                $scope.fetch();
            }).
            error(function (data, status) {
                $scope.stopUpdating(status);
            });
    };

    $scope.updateModel = function (method, url) {
        $scope.method = method;
        $scope.url = url;
    };

    $scope.stopUpdating = function (status) {
        $scope.running = false;
        $scope.status = status;
    };

    $scope.filterCharacters = function (entry) {
        return entry["isCharacter"];
    };

    $scope.colorStatus = function (status) {
        if (status.beginsWith("Blood")) {
            return "label label-important";
        } else if (status.beginsWith("Ok")) {
            return "label label-success";
        } else {
            return "label";
        }
    }

    String.prototype.beginsWith = function (string) {
        return(this.indexOf(string) === 0);
    };
}