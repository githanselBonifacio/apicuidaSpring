package co.com.sura.autoagendar;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Individuo implements Comparable<Individuo>, Cloneable{
    private List<List<CitaGenetic>> citaGen;

    public Individuo(Individuo individuo) {
        this.citaGen = individuo.citaGen;
    }

    @Override
    public int compareTo(Individuo o) {
        return this.citaGen.get(0).get(0).compareTo(o.citaGen.get(0).get(0));

    }
    public void sortCitaGen() {
        for (List<CitaGenetic> sublist : citaGen) {
            Collections.sort(sublist, new Comparator<CitaGenetic>() {

                @Override
                public int compare(CitaGenetic o1, CitaGenetic o2) {
                    return o1.compareTo(o2);
                }
            });
        }
    }

    @Override
    protected Individuo clone() {
        try {
            Individuo cloned = (Individuo) super.clone();
            List<List<CitaGenetic>> clonedCitaGen = new ArrayList<>();
            for (List<CitaGenetic> citas : citaGen) {
                List<CitaGenetic> clonedCitas = new ArrayList<>();
                for (CitaGenetic cita : citas) {
                    clonedCitas.add(cita.clone());
                }
                clonedCitaGen.add(clonedCitas);
            }
            cloned.citaGen = clonedCitaGen;
            return cloned;
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }
}
