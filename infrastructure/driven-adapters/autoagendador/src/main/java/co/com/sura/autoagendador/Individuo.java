package co.com.sura.autoagendador;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import reactor.core.publisher.Flux;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Individuo implements Comparable<Individuo>, Cloneable{

    private Flux<List<CitaGenetic>> citaGen;

    @Override
    public int compareTo(Individuo otroIndividuo) {
        var otrasCitasGen = otroIndividuo.citaGen.collectList().block();
        return Objects.requireNonNull(
                this.citaGen.collectList().block()).get(0).get(0).compareTo(
                Objects.requireNonNull(otrasCitasGen).get(0).get(0)
        );
    }
    public void sortCitaGen() {
        List<List<CitaGenetic>> citasGenList = citaGen.collectList().block();
        if (citasGenList != null && !citasGenList.isEmpty()) {
            citasGenList.get(0).sort(CitaGenetic::compareTo);
        }
    }

    @Override
    protected Individuo clone() {
        try {
            Individuo cloned = (Individuo) super.clone();
            cloned.citaGen = this.citaGen.map(ArrayList::new);
            return cloned;
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }

    @Override
    public String toString() {
        return "Individuo{" +
                "citaGen=" + citaGen +
                '}';
    }
}
