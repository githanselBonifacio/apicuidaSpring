package co.com.sura.autoagendador;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
public class Poblacion implements Cloneable{
    private List<Individuo> individuos;

    public Poblacion() {
        this.individuos = new ArrayList<>();
    }

    @Override
    protected Poblacion clone()  {
        Poblacion cloned;
        try {
            cloned = (Poblacion) super.clone();
        } catch (CloneNotSupportedException e) {
            throw  new AssertionError();
        }
        cloned.individuos = new ArrayList<>(this.individuos);
        return cloned;
    }
}
