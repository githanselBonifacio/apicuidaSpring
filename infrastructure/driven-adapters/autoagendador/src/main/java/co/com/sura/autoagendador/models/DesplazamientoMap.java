package co.com.sura.autoagendador.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class DesplazamientoMap {
    private String pos1;
    private String pos2;
    private Integer tiempo;
    private LocalDate fecha;
}
