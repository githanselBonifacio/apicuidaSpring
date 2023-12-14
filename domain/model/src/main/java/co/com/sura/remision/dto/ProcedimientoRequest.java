package co.com.sura.remision.dto;

import co.com.sura.remision.entity.procedimientos.Canalizacion;
import co.com.sura.remision.entity.procedimientos.Fototerapia;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class ProcedimientoRequest {
    private List<CuracionRequest> curaciones;
    private List<Fototerapia> fototerapias;
    private List<SondajeRequest> sondajes;
    private List<SecrecionRequest> secreciones;
    private List<TomaMuestraRequest> tomaMuestras;
    private List<SoporteNutricionalRequest> soporteNutricionales;
    private List<Canalizacion> canalizaciones;

}
