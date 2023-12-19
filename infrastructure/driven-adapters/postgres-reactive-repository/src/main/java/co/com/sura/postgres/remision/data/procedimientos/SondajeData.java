package co.com.sura.postgres.remision.data.procedimientos;

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
@Table(name = "sondajes")
public class SondajeData {
    @Id
    @Column("id_sondaje")
    private Integer idSondaje;

    @Column("tipo_sondaje")
    private String tipoSondaje;

    @Column("tipo_sonda")
    private String tipoSonda;

    @Column("total_sesiones")
    private Integer totalSesiones;

    @Column("tipo_prestacion")
    private String tipoPrestacion;

    @Column("id_cita")
    private String idCita;
}
