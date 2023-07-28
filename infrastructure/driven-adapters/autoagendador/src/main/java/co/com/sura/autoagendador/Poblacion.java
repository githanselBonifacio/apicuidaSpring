package co.com.sura.autoagendador;

import lombok.AllArgsConstructor;
import lombok.Data;
import reactor.core.publisher.Flux;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
public class Poblacion implements Cloneable{
    private Flux<Individuo> individuos;

    public Poblacion() {
        this.individuos = Flux.empty();
    }

    @Override
    protected Poblacion clone()  {
        Poblacion cloned;
        try {
            cloned = (Poblacion) super.clone();
        } catch (CloneNotSupportedException e) {
            throw  new AssertionError();
        }
        cloned.individuos = Flux.fromIterable(this.individuos.toIterable());
        return cloned;
    }

    @Override
    public String toString() {
        return "Poblacion{" +
                "individuos=" + individuos +
                '}';
    }
}
