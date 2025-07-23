(define-module (examples restaurant static-architecture)
  #:use-module (cpnet core)
  #:use-module (cpnet architecture)
  #:export (;; Sales Component Interface
            SalesNet
            sale-event
            public-dish-sold

            ;; Inventory Component Interface
            InventoryNet
            public-ingredients-decrement
            out-low-stock-trigger

            ;; RecipeBook Component Interface
            RecipeBookNet
            in-dish-name
            out-ingredients
            in-new-recipe

            ;; Forecasting Component Interface
            ForecastingNet
            in-forecast-dish
            out-ingredient-forecast

            ;; Top-level event for adding new recipes
            new-recipe-event

            ))

;;; ======================================================================
;;; 정적 아키텍처 (Static Architecture)
;;;
;;; 시스템의 컴포넌트(CPNET)들과 그들의 공개 인터페이스(단말 셀)를 정의합니다.
;;; 이것은 컴포넌트의 "설계도" 역할을 하며, 내부 구현은 포함하지 않습니다.
;;; ======================================================================

;;; --- 컴포넌트 정의 ---
(define SalesNet-iface (make-component-interface '(sale-event public-dish-sold)))
(define SalesNet (interface-net SalesNet-iface))
(define sale-event (cell-ref SalesNet-iface 'sale-event))
(define public-dish-sold (cell-ref SalesNet-iface 'public-dish-sold))

(define InventoryNet-iface (make-component-interface '(public-ingredients-decrement out-low-stock-trigger)))
(define InventoryNet (interface-net InventoryNet-iface))
(define public-ingredients-decrement (cell-ref InventoryNet-iface 'public-ingredients-decrement))
(define out-low-stock-trigger (cell-ref InventoryNet-iface 'out-low-stock-trigger))

(define RecipeBookNet-iface (make-component-interface '(in-dish-name out-ingredients in-new-recipe)))
(define RecipeBookNet (interface-net RecipeBookNet-iface))
(define in-dish-name (cell-ref RecipeBookNet-iface 'in-dish-name))
(define out-ingredients (cell-ref RecipeBookNet-iface 'out-ingredients))
(define in-new-recipe (cell-ref RecipeBookNet-iface 'in-new-recipe))

(define ForecastingNet-iface (make-component-interface '(in-forecast-dish out-ingredient-forecast)))
(define ForecastingNet (interface-net ForecastingNet-iface))
(define in-forecast-dish (cell-ref ForecastingNet-iface 'in-forecast-dish))
(define out-ingredient-forecast (cell-ref ForecastingNet-iface 'out-ingredient-forecast))

;;; --- 시뮬레이션을 위한 최상위 이벤트 ---
(define new-recipe-event (make-cell 'new-recipe-event #f))


