package co.com.sura.postgres.repository.remision.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "toma_muestra")
public class TomaMuestraData {
    @Id
    @Column("id_toma_muestra")
    private Integer idtomaMuestra;

    @Column("tipo_muestra")
    private String tipoMuestra;

    @Column("requiere_ayuno")
    private Boolean requiereAyuno;

    @Column("tipo_prestacion")
    private String tipoPrestacion;

    @Column("id_cita")
    private String idCita;
}
