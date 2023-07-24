package co.com.sura.autoagendador;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Resultado {
    private Double puntajeAptitudGlobalIndividuo;
    private List<List<Integer>> puntajeAptitudIndividuo;
    private Individuo individuo;

}
