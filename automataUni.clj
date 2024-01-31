

(defn bin_dec [binario]
  (Integer/parseInt binario 2))

(defn dec_bin [num]
  (if (zero? num)
    '()
    (concat (dec_bin (quot num 2)) [(mod num 2)])))

(defn dec_bin2 [lista]
  (if (< (count lista) 8)
    (dec_bin2 (cons '0 lista))
    lista))

(defn num_list [num]
  (mapv #(Character/digit % 10) num))

(defn list_num [lista]
  (apply str (map str lista)))



(defn au2 [gen regla estadoIni]
  (let [numbin (reverse (dec_bin2 (dec_bin regla)))
        lista2 (num_list estadoIni)
        lista1 (conj (butlast lista2) (last lista2))
        lista3 (concat (next lista2) [(first lista2)])
        laux (mapv (fn [x y z]
                     (Integer/parseInt (str x y z)))
                   lista1 lista2 lista3)
        laux2 (mapv #(bin_dec (str %)) (mapv #(str %) laux))
        estadoNuevo (mapv #(nth numbin %) laux2)]
   
    (println estadoNuevo)
    (if (> gen 1)
      (au2 (dec gen) regla (list_num estadoNuevo))
      (println "FIN DEL PROCESO"))))



(au2 10 30 "0000000000000000000000000000111")