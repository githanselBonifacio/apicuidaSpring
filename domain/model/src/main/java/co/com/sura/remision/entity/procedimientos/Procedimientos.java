package co.com.sura.remision.entity.procedimientos;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Procedimientos {
    private List<Curacion> curaciones;
    private List<Fototerapia> fototerapias;
    private List<Sondaje> sondajes;
    private List<Secrecion> secreciones;
    private List<TomaMuestra> tomaMuestras;
    private List<SoporteNutricional> soporteNutricionales;
    private List<Canalizacion> canalizaciones;

}
